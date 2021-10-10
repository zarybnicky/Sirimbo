{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Olymp.API.Announcement
  ( AnnouncementAPI,
    announcementAPI,
  )
where

import Data.Int (Int64)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Database.Esqueleto (LeftOuterJoin (..), from, limit, offset, on, select, (==.), (^.))
import Database.Persist.Sql (Key, SqlBackend, count)
import qualified Database.Persist.Sql as Persistent
import Olymp.Prelude
import Olymp.Schema (Announcement (..), AnnouncementGroup (..), AnnouncementId)

type AnnouncementAPI =
  PhpAuth :> "announcement" :> QueryParam "offset" Int64
    :> QueryParam "limit" Int64
    :> OpId "getAnnouncements"
    :> Get '[JSON] (PagedResponse [AnnouncementResponse])
      :<|> PhpAuth
    :> "announcement"
    :> Capture "id" AnnouncementId
    :> OpId "getAnnouncement"
    :> Get '[JSON] AnnouncementResponse

data PagedResponse x = PagedResponse
  { items :: x,
    total :: Int
  }
  deriving (Generic)

instance ToJSON a => ToJSON (PagedResponse a)

instance ToSchema a => ToSchema (PagedResponse a)

data AnnouncementResponse = AnnouncementResponse
  { announcement :: Announcement,
    groups :: [AnnouncementGroup]
  }
  deriving (Generic, ToJSON, ToSchema)

announcementAPI :: Effs '[Error ServerError, Database] m => ServerT AnnouncementAPI m
announcementAPI = getAnnouncements :<|> getAnnouncement

getAnnouncements ::
  Effs '[Error ServerError, Database] m =>
  (SessionId, Entity User) ->
  Maybe Int64 ->
  Maybe Int64 ->
  m (PagedResponse [AnnouncementResponse])
getAnnouncements _ mOffset mLimit = do
  total <- query $ count @SqlBackend @IO @Announcement []
  notGrouped <- query $
    select $
      from $ \(ann `LeftOuterJoin` group) -> do
        on (ann ^. AnnouncementId ==. group ^. AnnouncementGroupParent)
        limit (fromMaybe 10 mLimit)
        offset (fromMaybe 0 mOffset)
        return (ann, group)
  pure $ PagedResponse (uncurry AnnouncementResponse . bimap entityVal (fmap entityVal) <$> groupData notGrouped) total

getAnnouncement ::
  Effs '[Error ServerError, Database] m =>
  (SessionId, Entity User) ->
  AnnouncementId ->
  m AnnouncementResponse
getAnnouncement _ k = do
  announcement <- maybe (throw err404) pure =<< query (get k)
  groups <- query $ selectList [AnnouncementGroupParent Persistent.==. k] []
  pure $ AnnouncementResponse announcement (entityVal <$> groups)

groupData :: (Ord (Key a)) => [(Entity a, b)] -> [(Entity a, [b])]
groupData res =
  M.elems $ foldr (\(a, b) -> M.insertWith mergeData (entityKey a) (a, [b])) M.empty res

mergeData :: (a, [b]) -> (a, [b]) -> (a, [b])
mergeData (a, b) (_, b') = (a, b ++ b')
