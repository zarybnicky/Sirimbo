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

module Olymp.YouTube.Worker
  ( WorkerScope
  , youtubeWorker
  , redirectPrompt
  , getYTToken
  , loadChannels
  , loadUploads
  , loadPlaylistsForChannel
  , checkNewVideos
  , checkPlaylistMappings
  , listVideosForChannel
  , printVideosForChannel
  , getVideosForPlaylist
  ) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Effect (Threaders, Carrier, Eff, Effs, Embed, embed)
import Control.Effect.Error (ErrorThreads, throw, runThrow)
import Control.Effect.Writer (WriterThreads, TellC, runTell, tell)
import Control.Monad (void, forM_, foldM, forM)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (getCurrentTime)
import Database.Persist
  ((<-.), (=.), (==.), Entity(..), Key, Update(..), count, insertMany, insert, selectList, update, updateWhere)
import Network.Google (LogLevel(Debug), tlsManagerSettings, newManager, newLogger)
import Network.Google.Auth
import Network.Google.YouTube
import Olymp.Effect.Database (Database, query)
import Olymp.Effect.Google (GoogleEff, sendG)
import Olymp.Schema (EntityField(..), VideoList(..), VideoSource(..), videoUri)
import qualified Olymp.Schema as Schema
import System.Exit (exitFailure)
import System.IO (stdout)
import System.Info (os)
import System.Process (rawSystem)

type WorkerScope = '[ "https://www.googleapis.com/auth/youtube.readonly"]

youtubeWorker
  :: (Effs '[GoogleEff WorkerScope, Database, Embed IO] m, Threaders '[ErrorThreads, WriterThreads] m p)
  => m ()
youtubeWorker = do
  embed $ threadDelay 100000000
  checkNewVideos
  checkPlaylistMappings

printVideosForChannel
  :: (Effs '[GoogleEff WorkerScope, Embed IO] m, Threaders '[ErrorThreads, WriterThreads] m p)
  => Text -> m ()
printVideosForChannel channelId = do
  m <- listVideosForChannel channelId
  forM_ m $ \(pl, plis) -> do
    embed . print $ pl ^. plsTitle . _Just
    forM_ plis $ \v -> do
      embed . print $ v ^. plisTitle . _Just
      embed . print $ v ^. plisDescription . _Just

checkNewVideos
  :: (Effs '[GoogleEff WorkerScope, Database, Embed IO] m, Threaders '[ErrorThreads, WriterThreads] m p)
  => m ()
checkNewVideos = do
  chs <- loadChannels
  newVids <- loadUploads chs
  void . query $ insertMany newVids

checkPlaylistMappings
  :: (Effs '[Database, GoogleEff WorkerScope, Embed IO] m, Threaders '[WriterThreads] m p)
  => m ()
checkPlaylistMappings = do
  chans <- fmap (videoSourceUrl . entityVal) <$> query (selectList [] [])
  forM_ chans $ \chan -> do
    lists <- loadPlaylistsForChannel chan
    forM_ lists $ \list -> do
      vids <- mapVideosToPlaylists list Nothing
      forM_ vids $ \(vidIds, listUrl) ->
        query $ updateWhere [VideoUri <-. vidIds] [VideoPlaylistId =. Just listUrl]

listVideosForChannel
  :: (Eff (GoogleEff WorkerScope) m, Threaders '[ErrorThreads, WriterThreads] m p)
  => Text -> m (Map Text (PlayListSnippet, [PlayListItemSnippet]))
listVideosForChannel chId = do
  playlists <- getPlaylistsForChannel chId Nothing
  foldM
    (\m playlist -> case playlist ^. plSnippet of
      Nothing -> pure m
      Just snippet -> do
        let pid = playlist ^. plId . _Just
        vids <- getVideosForPlaylist pid Nothing
        pure $ M.insert pid (snippet, vids) m)
    M.empty playlists

mapVideosToPlaylists
  :: (Effs '[GoogleEff WorkerScope] m, Threaders '[WriterThreads] m p)
  => Schema.VideoList -> Maybe Text -> m [([Text], Text)]
mapVideosToPlaylists dbList = withPaging $ \pageToken -> do
  vs <- sendG @WorkerScope $ playListItemsList "snippet"
    & plilPlayListId ?~ videoListUrl dbList
    & plilPageToken .~ pageToken
    & plilMaxResults .~ 20
  let ids = vs ^.. plilrItems . each . pliSnippet . _Just . plisResourceId . _Just . riVideoId . _Just
  tell [(ids, videoListUrl dbList)]
  pure (vs ^. plilrNextPageToken)

loadPlaylistsForChannel
  :: (Effs '[Database, Embed IO, GoogleEff WorkerScope] m, Threaders '[WriterThreads] m p)
  => Text -> m [Schema.VideoList]
loadPlaylistsForChannel chanId = do
  playlists <- getPlaylistsForChannel chanId Nothing

  dbPlaylists :: [Entity Schema.VideoList] <- query $ selectList [] []

  x <- embed $ newIORef []
  forM_ playlists $ \p -> do
    let plId_ = p ^. plId . _Just
    let plcd = p ^. plContentDetails
    let plCount = maybe 0 fromIntegral $ maybe Nothing (^. plcdItemCount) plcd :: Int
    case find ((== p ^. plId) . Just . videoListUrl . entityVal) dbPlaylists of
      Nothing -> do
        now <- embed getCurrentTime
        let plTitle = p ^. plSnippet . _Just . plsTitle . _Just
        let plDesc = p ^. plSnippet . _Just . plsDescription . _Just
        let dbList = VideoList plId_ plTitle plDesc plCount now (Just now)
        _ <- query $ insert dbList
        embed $ modifyIORef' x (dbList:)

      Just dbList ->
        if videoListItemCount (entityVal dbList) /= plCount
          then embed $ modifyIORef' x (entityVal dbList:)
          else do
            cnt <- query $ count [VideoPlaylistId ==. Just plId_]
            if cnt /= plCount
              then embed $ modifyIORef' x (entityVal dbList:)
              else pure ()
  embed $ readIORef x

getPlaylistsForChannel
  :: (Eff (GoogleEff WorkerScope) m, Threaders '[WriterThreads] m p)
  => Text -> Maybe Text -> m [PlayList]
getPlaylistsForChannel chanId = withPaging $ \pageToken -> do
  lists <- sendG @WorkerScope $ playListsList "snippet,contentDetails"
    & pllChannelId ?~ chanId
    & pllPageToken .~ pageToken
  tell (lists ^. pllrItems)
  pure (lists ^. pllrNextPageToken)

loadUploads
  :: (Effs '[Embed IO, GoogleEff WorkerScope, Database] m, Threaders '[ErrorThreads, WriterThreads] m p)
  => [(Key VideoSource, Text)] -> m [Schema.Video]
loadUploads uploadIds = do
  videos :: [Entity Schema.Video] <- query $ selectList [] []
  let videoSet = S.fromList $ videoUri . entityVal <$> videos

  x <- embed $ newIORef []
  forM_ uploadIds $ \(ch, playlist) -> do
    items <- fmap snd . runTell @[Video] $ loadNewVideosFromPlaylist videoSet playlist Nothing
    embed $ modifyIORef' x (items ++)
    now <- embed getCurrentTime
    query $ update ch [VideoSourceLastCheckedAt =. Just now]
  embed $ readIORef x

loadNewVideosFromPlaylist ::
     (Effs '[GoogleEff WorkerScope, Embed IO] m, Threaders '[ErrorThreads, WriterThreads] m p)
  => Set Text
  -> Text
  -> Maybe Text
  -> m [Schema.Video]
loadNewVideosFromPlaylist currentVids playlist =
  withPaging $ \pageToken -> fmap hush . runThrow @Text $ do
    vs <- sendG @WorkerScope $ playListItemsList "snippet"
      & plilPlayListId ?~ playlist
      & plilPageToken .~ pageToken
      & plilMaxResults .~ 20
    forM_ (vs ^.. plilrItems . each . pliSnippet . _Just) $ \v -> do
      now <- embed getCurrentTime
      let vidId = v ^. plisResourceId . _Just . riVideoId . _Just
      let vidTitle = v ^. plisTitle . _Just
      let vidDesc = v ^. plisDescription . _Just
      let vidChannel = v ^. plisChannelTitle . _Just
      if vidId `S.member` currentVids
        then throw ("Found an existing video, exiting" :: Text)
        else tell [Schema.Video vidId vidTitle vidChannel vidDesc Nothing now now]
    pure (vs ^. plilrNextPageToken)
  where
    hush = either (const Nothing) id

getVideosForPlaylist ::
     (Eff (GoogleEff WorkerScope) m, Threaders '[WriterThreads] m p)
  => Text
  -> Maybe Text
  -> m [PlayListItemSnippet]
getVideosForPlaylist playlistId = withPaging $ \pageToken -> do
  vs <- sendG @WorkerScope $ playListItemsList "snippet"
    & plilPlayListId ?~ playlistId
    & plilPageToken .~ pageToken
    & plilMaxResults .~ 20
  tell (vs ^.. plilrItems . each . pliSnippet . _Just)
  pure (vs ^. plilrNextPageToken)

loadChannels :: Effs '[GoogleEff WorkerScope, Database] m => m [(Key VideoSource, Text)]
loadChannels = do
  channels <- query $ selectList [] []
  let chanIds = T.intercalate "," $ videoSourceUrl . entityVal <$> channels
  resp <- sendG @WorkerScope $ channelsList "contentDetails,snippet" & cId ?~ chanIds
  fmap catMaybes . forM (resp ^. clrItems) $ \x ->
    case find ((== x ^. chaId) . Just . videoSourceUrl . entityVal) channels of
      Nothing -> pure Nothing
      Just dbChan -> do
        let chTitle = x ^. chaSnippet . _Just . csTitle
        let chDesc = x ^. chaSnippet . _Just . csDescription
        case fillInChannel chTitle chDesc dbChan of
          Nothing -> pure ()
          Just (key, updates) -> query $ update key updates
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

withPaging :: (Carrier m, Monoid b, Threaders '[WriterThreads] m p) => (Maybe a -> TellC b m (Maybe a)) -> Maybe a -> m b
withPaging f = fmap fst . runTell . go
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
  code <- redirectPrompt @WorkerScope c
  auth <- exchange (FromClient c code) lgr mgr
  pure (_tokenRefresh (_token auth))

redirectPrompt :: forall s. AllowScopes s => OAuthClient -> IO (OAuthCode s)
redirectPrompt c = do
  let url = formURL c (Proxy @s)
  T.putStrLn $ "Opening URL " <> url
  _ <- case os of
    "darwin" -> rawSystem "open"     [T.unpack url]
    "linux"  -> rawSystem "xdg-open" [T.unpack url]
    _        -> T.putStrLn "Unsupported OS" >> exitFailure
  T.putStrLn "Please input the authorisation code: "
  OAuthCode <$> T.getLine
