{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main
  , redirectPrompt
  , getYTToken
  , loadChannels
  , loadUploads
  , loadPlaylistsForChannel
  , checkNewVideos
  , checkPlaylistMappings
  , listVideosForChannel
  , getVideosForPlaylist
  , migrateAll
  ) where

import Control.Lens
import Control.Monad (forM, foldM)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Control.Monad.Reader
import Control.Monad.Trans.Resource (ResourceT, MonadResource)
import Control.Monad.Writer
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as BC
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (getCurrentTime)
import Data.Yaml (decodeFileThrow)
import Database.Persist
import Database.Persist.MySQL
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Network.Google
import Network.Google.Auth
import Network.Google.YouTube
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (stdout)
import System.Info (os)
import System.Process (rawSystem)

import Schema
  ( EntityField(..)
  , VideoList(..)
  , VideoSource(..)
  , migrateAll
  , videoUri
  )
import qualified Schema (Video(Video))

type AppScope = '[ "https://www.googleapis.com/auth/youtube.readonly"]
type MonadMysql m = (MonadUnliftIO m, MonadLogger m, HasPool m)

data AppEnv = AppEnv
  { _env :: Env AppScope
  , _pool :: ConnectionPool
  }

class HasPool m where
  getPool :: m ConnectionPool

instance Monad m => HasPool (ReaderT AppEnv m) where
  getPool = asks _pool

instance HasEnv AppScope AppEnv where
  environment = lens _env (\x p -> x { _env = p })
instance {-# OVERLAPPING #-} (Monad m, MonadIO m, MonadCatch m, MonadResource m) => MonadGoogle AppScope (ReaderT AppEnv m) where
  liftGoogle f = asks _env >>= flip runGoogle f

instance MonadGoogle s m => MonadGoogle s (ResourceT m) where
  liftGoogle = lift . liftGoogle

data AppConfig = AppConfig
  { dbHost :: String
  , dbUser :: String
  , dbPassword :: String
  , dbDatabase :: String
  } deriving (Show, Generic, FromJSON)

liftMysql :: (MonadUnliftIO m, HasPool m) => ReaderT SqlBackend m a -> m a
liftMysql f = runSqlPool f =<< getPool

main :: IO ()
main = do
  lgr <- newLogger Network.Google.Info stdout
  mgr <- liftIO $ newManager tlsManagerSettings
  cred <- getApplicationDefault mgr
  env <- newEnvWith cred lgr mgr <&> (envScopes .~ youTubeReadOnlyScope)

  configFile <- fromMaybe "config.yaml" <$> lookupEnv "CONFIG"
  putStrLn $ "Reading config from: " <> configFile
  AppConfig{..} <- decodeFileThrow configFile

  let connectInfo =
        mkMySQLConnectInfo dbHost (BC.pack dbUser) (BC.pack dbPassword) (BC.pack dbDatabase)

  runResourceT . runStdoutLoggingT . withMySQLPool connectInfo 1 $
    \pool -> flip runReaderT (AppEnv env pool) $ do
      checkNewVideos
      checkPlaylistMappings
      -- m <- listVideosForChannel channelId
      -- forM_ m $ \(pl, plis) -> do
      --   liftIO . print $ pl ^. plsTitle . _Just
      --   forM_ plis $ \v -> do
      --     liftIO . print $ v ^. plisTitle . _Just
      --     liftIO . print $ v ^. plisDescription . _Just

checkNewVideos :: (MonadMysql m, MonadGoogle AppScope m) => m ()
checkNewVideos = do
  chs <- loadChannels
  newVids <- loadUploads chs
  void . liftMysql $ insertMany newVids

checkPlaylistMappings :: (MonadMysql m, MonadGoogle AppScope m) => m ()
checkPlaylistMappings = do
  chans <- fmap (videoSourceUrl . entityVal) <$> liftMysql (selectList [] [])
  forM_ chans $ \chan -> do
    lists <- loadPlaylistsForChannel chan
    forM_ lists $ \list -> do
      vids <- mapVideosToPlaylists list Nothing
      forM_ vids $ \(vidIds, listUrl) ->
        liftMysql $ updateWhere [VideoUri <-. vidIds] [VideoPlaylistId =. Just listUrl]

listVideosForChannel :: MonadGoogle AppScope m => Text -> m (Map Text (PlayListSnippet, [PlayListItemSnippet]))
listVideosForChannel chId = do
  playlists <- getPlaylistsForChannel chId Nothing
  foldM getPlaylist M.empty playlists
  where
    getPlaylist m playlist = case playlist ^. plSnippet of
      Nothing -> pure m
      Just snippet -> do
        let pid = playlist ^. plId . _Just
        vids <- getVideosForPlaylist pid Nothing
        pure $ M.insert pid (snippet, vids) m

mapVideosToPlaylists :: MonadGoogle AppScope m => Schema.VideoList -> Maybe Text -> m [([Text], Text)]
mapVideosToPlaylists dbList = withPaging $ \pageToken -> do
  vs <- send $ playListItemsList "snippet"
    & plilPlayListId ?~ videoListUrl dbList
    & plilPageToken .~ pageToken
    & plilMaxResults .~ 20
  let ids = vs ^.. plilrItems . each . pliSnippet . _Just . plisResourceId . _Just . riVideoId . _Just
  tell [(ids, videoListUrl dbList)]
  pure (vs ^. plilrNextPageToken)

loadPlaylistsForChannel :: (MonadMysql m, MonadGoogle AppScope m) => Text -> m [Schema.VideoList]
loadPlaylistsForChannel chanId = do
  playlists <- getPlaylistsForChannel chanId Nothing

  dbPlaylists :: [Entity Schema.VideoList] <- liftMysql $ selectList [] []

  x <- liftIO $ newIORef []
  forM_ playlists $ \p -> do
    let plId_ = p ^. plId . _Just
    let plcd = p ^. plContentDetails
    let plCount = maybe 0 fromIntegral $ maybe Nothing (^. plcdItemCount) plcd :: Int
    case find ((== p ^. plId) . Just . videoListUrl . entityVal) dbPlaylists of
      Nothing -> do
        now <- liftIO getCurrentTime
        let plTitle = p ^. plSnippet . _Just . plsTitle . _Just
        let plDesc = p ^. plSnippet . _Just . plsDescription . _Just
        let dbList = VideoList plId_ plTitle plDesc plCount now (Just now)
        _ <- liftMysql $ insert dbList
        liftIO $ modifyIORef' x (dbList:)

      Just dbList ->
        if videoListItemCount (entityVal dbList) /= plCount
          then liftIO $ modifyIORef' x (entityVal dbList:)
          else do
            cnt <- liftMysql $ count [VideoPlaylistId ==. Just plId_]
            if cnt /= plCount
              then liftIO $ modifyIORef' x (entityVal dbList:)
              else pure ()
  liftIO $ readIORef x

getPlaylistsForChannel ::
     MonadGoogle AppScope m => Text -> Maybe Text -> m [PlayList]
getPlaylistsForChannel chanId = withPaging $ \pageToken -> do
  lists <- send $ playListsList "snippet,contentDetails"
    & pllChannelId ?~ chanId
    & pllPageToken .~ pageToken
  tell (lists ^. pllrItems)
  pure (lists ^. pllrNextPageToken)

loadUploads :: (MonadMysql m, MonadGoogle AppScope m) => [(Key VideoSource, Text)] -> m [Schema.Video]
loadUploads uploadIds = do
  videos :: [Entity Schema.Video] <- liftMysql $ selectList [] []
  let videoSet = S.fromList $ videoUri . entityVal <$> videos

  x <- liftIO $ newIORef []
  forM_ uploadIds $ \(ch, playlist) -> do
    items <- fmap snd . runWriterT $ loadNewVideosFromPlaylist videoSet playlist Nothing
    liftIO $ modifyIORef' x (items ++)
    now <- liftIO getCurrentTime
    liftMysql $ update ch [VideoSourceLastCheckedAt =. Just now]
  liftIO $ readIORef x

loadNewVideosFromPlaylist ::
     MonadGoogle AppScope m
  => Set Text
  -> Text
  -> Maybe Text
  -> m [Schema.Video]
loadNewVideosFromPlaylist currentVids playlist =
  withPaging $ \pageToken -> fmap hush . runExceptT $ do
    vs <- send $ playListItemsList "snippet"
      & plilPlayListId ?~ playlist
      & plilPageToken .~ pageToken
      & plilMaxResults .~ 20
    forM_ (vs ^.. plilrItems . each . pliSnippet . _Just) $ \v -> do
      now <- liftIO getCurrentTime
      let vidId = v ^. plisResourceId . _Just . riVideoId . _Just
      let vidTitle = v ^. plisTitle . _Just
      let vidDesc = v ^. plisDescription . _Just
      let vidChannel = v ^. plisChannelTitle . _Just
      if vidId `S.member` currentVids
        then throwError ("Found an existing video, exiting" :: Text)
        else tell [Schema.Video vidId vidTitle vidChannel vidDesc Nothing now now]
    pure (vs ^. plilrNextPageToken)
  where
    hush = either (const Nothing) id

getVideosForPlaylist ::
     MonadGoogle AppScope m
  => Text
  -> Maybe Text
  -> m [PlayListItemSnippet]
getVideosForPlaylist playlistId = withPaging $ \pageToken -> do
  vs <- send $ playListItemsList "snippet"
    & plilPlayListId ?~ playlistId
    & plilPageToken .~ pageToken
    & plilMaxResults .~ 20
  tell (vs ^.. plilrItems . each . pliSnippet . _Just)
  pure (vs ^. plilrNextPageToken)

loadChannels :: (MonadMysql m, MonadGoogle AppScope m) => m [(Key VideoSource, Text)]
loadChannels = do
  channels <- liftMysql $ selectList [] []
  let chanIds = T.intercalate "," $ videoSourceUrl . entityVal <$> channels
  resp <- send $ channelsList "contentDetails,snippet" & cId ?~ chanIds
  fmap catMaybes . forM (resp ^. clrItems) $ \x ->
    case find ((== x ^. chaId) . Just . videoSourceUrl . entityVal) channels of
      Nothing -> pure Nothing
      Just dbChan -> do
        let chTitle = x ^. chaSnippet . _Just . csTitle
        let chDesc = x ^. chaSnippet . _Just . csDescription
        case fillInChannel chTitle chDesc dbChan of
          Nothing -> pure ()
          Just (key, updates) -> liftMysql $ update key updates
        pure $ Just (entityKey dbChan, x ^. chaContentDetails . _Just . ccdRelatedPlayLists . _Just . ccdrplUploads . _Just)

fillInChannel ::
     Maybe Text
  -> Maybe Text
  -> Entity VideoSource
  -> Maybe (Key VideoSource, [Update VideoSource])
fillInChannel realTitle realDesc Entity{..} =
  case (videoSourceTitle entityVal, videoSourceDescription entityVal) of
    (Just _, Just _) -> Nothing
    _ -> Just (entityKey, [VideoSourceTitle =. realTitle, VideoSourceDescription =. realDesc])

-- printMigrations :: IO ()
-- printMigrations = runMysql $ printMigration migrateAll

withPaging :: (Monad m, Monoid a) => (Maybe t -> WriterT a m (Maybe t)) -> Maybe t -> m a
withPaging f = fmap snd . runWriterT . go
  where
    go t = do
      r <- f t
      case r of
        Nothing -> pure ()
        Just t' -> go (Just t')

getYTToken ::
     ClientId
  -> GSecret
  -> IO (Maybe RefreshToken)
getYTToken cid secret = do
  let c = OAuthClient cid secret
  lgr <- newLogger Debug stdout
  mgr <- newManager tlsManagerSettings
  code <- redirectPrompt @AppScope c
  auth <- exchange (FromClient c code) lgr mgr
  pure (_tokenRefresh (_token auth))

redirectPrompt :: forall s. AllowScopes (s :: [Symbol]) => OAuthClient -> IO (OAuthCode s)
redirectPrompt c = do
  let url = formURL c (Proxy @s)
  T.putStrLn $ "Opening URL " <> url
  _ <- case os of
    "darwin" -> rawSystem "open"     [T.unpack url]
    "linux"  -> rawSystem "xdg-open" [T.unpack url]
    _        -> T.putStrLn "Unsupported OS" >> exitFailure
  T.putStrLn "Please input the authorisation code: "
  OAuthCode <$> T.getLine
