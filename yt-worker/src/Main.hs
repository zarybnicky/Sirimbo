{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  , printMigrations
  , redirectPrompt
  , getYTToken
  , loadChannels
  , loadUploads
  , loadPlaylistsForChannel
  , checkNewVideos
  , checkPlaylistMappings
  ) where

import Control.Lens
import Control.Monad (forM)
import Control.Monad.Except
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.MySQL
  (ConnectInfo(..), SqlBackend, defaultConnectInfo, printMigration, runSqlPool, withMySQLPool)
import GHC.TypeLits (Symbol)
import Network.Google
import Network.Google.Auth
import Network.Google.YouTube
import System.Exit (exitFailure)
import System.IO (stdout)
import System.Info (os)
import System.Process (rawSystem)

import Schema

main :: IO ()
main = pure ()

checkNewVideos :: IO ()
checkNewVideos = do
  chs <- loadChannels
  newVids <- loadUploads chs
  void . runMysql $ insertMany newVids

checkPlaylistMappings :: IO ()
checkPlaylistMappings = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ youTubeReadOnlyScope)
  channels <- runMysql $ selectList [] []
  forM_ (videoSourceUrl . entityVal <$> channels) $ \chanId -> do
    lists <- loadPlaylistsForChannel chanId
    runResourceT . runGoogle env $
      forM lists (`mapVideosToPlaylists` Nothing)

mapVideosToPlaylists ::
     Schema.VideoList
  -> Maybe Text
  -> Google '[ "https://www.googleapis.com/auth/youtube.readonly"] ()
mapVideosToPlaylists dbList pageToken = do
  vs <- send $ playListItemsList "snippet"
    & plilPlayListId ?~ videoListUrl dbList
    & plilPageToken .~ pageToken
    & plilMaxResults .~ 20
  let ids = vs ^.. plilrItems . each . pliSnippet . _Just . plisResourceId . _Just . riVideoId . _Just
  liftIO . runMysql $ updateWhere [VideoUri <-. ids] [VideoPlaylistId =. Just (videoListUrl dbList)]

  case vs ^. plilrNextPageToken of
    Nothing -> pure ()
    Just token -> mapVideosToPlaylists dbList (Just token)

loadPlaylistsForChannel :: Text -> IO [Schema.VideoList]
loadPlaylistsForChannel chanId = do
  now <- liftIO getCurrentTime
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ youTubeReadOnlyScope)
  playlists <- fmap snd . runResourceT . runGoogle env . runWriterT $
    getPlaylistsForChannel chanId Nothing

  dbPlaylists :: [Entity Schema.VideoList] <- runMysql $ selectList [] []

  fmap snd . runWriterT . forM playlists $ \p -> do
    let plId_ = p ^. plId . _Just
    let plcd = p ^. plContentDetails
    let plCount = maybe 0 fromIntegral $ maybe Nothing (^. plcdItemCount) plcd :: Int
    case find ((== p ^. plId) . Just . videoListUrl . entityVal) dbPlaylists of
      Nothing -> do
        let plTitle = p ^. plSnippet . _Just . plsTitle . _Just
        let plDesc = p ^. plSnippet . _Just . plsDescription . _Just
        let dbList = VideoList plId_ plTitle plDesc plCount now (Just now)
        _ <- liftIO $ runMysql $ insert dbList
        tell [dbList]

      Just dbList ->
        if videoListItemCount (entityVal dbList) /= plCount
          then tell [entityVal dbList]
          else do
            cnt <- liftIO . runMysql $ count [VideoPlaylistId ==. Just plId_]
            if cnt /= plCount
              then tell [entityVal dbList]
              else pure ()

getPlaylistsForChannel ::
     Text
  -> Maybe Text
  -> WriterT [PlayList] (Google '[ "https://www.googleapis.com/auth/youtube.readonly"]) ()
getPlaylistsForChannel chanId pageToken = do
  lists <- send $ playListsList "snippet,contentDetails"
    & pllChannelId ?~ chanId
    & pllPageToken .~ pageToken
  tell (lists ^. pllrItems)
  case lists ^. pllrNextPageToken of
    Nothing -> pure ()
    Just token -> getPlaylistsForChannel chanId (Just token)


loadUploads :: [(Key VideoSource, Text)] -> IO [Schema.Video]
loadUploads uploadIds = do
  now <- liftIO getCurrentTime
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ youTubeReadOnlyScope)
  videos :: [Entity Schema.Video] <- runMysql $ selectList [] []
  let videoSet = S.fromList $ videoUri . entityVal <$> videos

  fmap snd . runResourceT . runGoogle env . runWriterT . forM uploadIds $ \(ch, playlist) -> do
    loadNewVideosFromPlaylist videoSet now playlist Nothing
    liftIO $ runMysql $ update ch [VideoSourceLastCheckedAt =. Just now]

loadNewVideosFromPlaylist ::
     Set Text
  -> UTCTime
  -> Text
  -> Maybe Text
  -> WriterT [Schema.Video] (Google '[ "https://www.googleapis.com/auth/youtube.readonly"]) ()
loadNewVideosFromPlaylist currentVids now playlist pageToken = do
  nextPage <- runExceptT $ do
    vs <- send $ playListItemsList "snippet"
      & plilPlayListId ?~ playlist
      & plilPageToken .~ pageToken
      & plilMaxResults .~ 20
    forM_ (vs ^.. plilrItems . each . pliSnippet . _Just) $ \v -> do
      let vidId = v ^. plisResourceId . _Just . riVideoId . _Just
      let vidTitle = v ^. plisTitle . _Just
      let vidDesc = v ^. plisDescription . _Just
      let vidChannel = v ^. plisChannelTitle . _Just
      if vidId `S.member` currentVids
        then throwError ("Found an existing video, exiting" :: Text)
        else tell [Schema.Video vidId vidTitle vidChannel vidDesc Nothing now now]

    pure (vs ^. plilrNextPageToken)
  case nextPage ^? _Right . _Just of
    Nothing -> pure ()
    Just token -> loadNewVideosFromPlaylist currentVids now playlist (Just token)

loadChannels :: IO [(Key VideoSource, Text)]
loadChannels = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ youTubeReadOnlyScope)
  channels <- runMysql $ selectList [] []
  let chanIds = T.intercalate "," $ videoSourceUrl . entityVal <$> channels
  resp <- runResourceT $ runGoogle env $ send $ channelsList "contentDetails,snippet" & cId ?~ chanIds
  fmap catMaybes . runMysql . forM (resp ^. clrItems) $ \x ->
    case find ((== x ^. chaId) . Just . videoSourceUrl . entityVal) channels of
      Nothing -> pure Nothing
      Just dbChan -> do
        let chTitle = x ^. chaSnippet . _Just . csTitle
        let chDesc = x ^. chaSnippet . _Just . csDescription
        case fillInChannel chTitle chDesc dbChan of
          Nothing -> pure ()
          Just (key, updates) -> update key updates
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


runMysql :: ReaderT SqlBackend (LoggingT IO) a -> IO a
runMysql f = runStdoutLoggingT . withMySQLPool connectInfo 1 $ runSqlPool f
  where
    connectInfo =
      defaultConnectInfo
        { connectHost = "127.0.0.1"
        , connectUser = "olymp"
        , connectPassword = "admin"
        , connectDatabase = "olymp"
        }

printMigrations :: IO ()
printMigrations = runMysql $ printMigration migrateAll

getYTToken ::
     AllowScopes (s :: [Symbol])
  => OAuthClient
  -> Logger
  -> proxy s
  -> IO (Maybe RefreshToken)
getYTToken c lgr p = do
  mgr <- newManager tlsManagerSettings
  code <- redirectPrompt c p
  auth <- exchange (FromClient c code) lgr mgr
  pure (_tokenRefresh (_token auth))

redirectPrompt :: AllowScopes (s :: [Symbol]) => OAuthClient -> proxy s -> IO (OAuthCode s)
redirectPrompt c p = do
  let url = formURL c p
  T.putStrLn $ "Opening URL " `T.append` url
  _ <- case os of
    "darwin" -> rawSystem "open"     [T.unpack url]
    "linux"  -> rawSystem "xdg-open" [T.unpack url]
    _        -> T.putStrLn "Unsupported OS" >> exitFailure
  T.putStrLn "Please input the authorisation code: "
  OAuthCode <$> T.getLine
