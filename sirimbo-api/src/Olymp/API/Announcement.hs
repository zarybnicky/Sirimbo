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
import Data.Maybe (fromMaybe, maybeToList, listToMaybe)
import Database.Esqueleto (InnerJoin (..), LeftOuterJoin (..), from, just, limit, offset, on, select, (==.), (?.), (^.), orderBy, desc, where_, val, SqlExpr, SqlQuery)
import Database.Persist.Sql (Key, SqlBackend, count)
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
  { announcementId :: AnnouncementId,
    announcement :: Announcement,
    user :: User,
    groups :: [AnnouncementGroup]
  }
  deriving (Generic, ToJSON, ToSchema)

announcementAPI :: Effs '[Error ServerError, Database] m => ServerT AnnouncementAPI m
announcementAPI = getAnnouncements :<|> getAnnouncement

announcementQuery :: SqlQuery ((SqlExpr (Entity Announcement), SqlExpr (Entity User)), SqlExpr (Maybe (Entity AnnouncementGroup)))
announcementQuery =
  from $ \(ann `InnerJoin` u `LeftOuterJoin` group) -> do
    on (ann ^. AnnouncementCreatedBy ==. u ^. UserId)
    on (just (ann ^. AnnouncementId) ==. group ?. AnnouncementGroupParent)
    orderBy [desc (ann ^. AnnouncementCreatedAt)]
    pure ((ann, u), group)

getAnnouncements ::
  Effs '[Error ServerError, Database] m =>
  (SessionId, Entity User) ->
  Maybe Int64 ->
  Maybe Int64 ->
  m (PagedResponse [AnnouncementResponse])
getAnnouncements _ mOffset mLimit = do
  xs <- fmap groupData . query $
    select $ do
      r <- announcementQuery
      limit (fromMaybe 10 mLimit)
      offset (fromMaybe 0 mOffset)
      return r
  total <- query $ count @SqlBackend @IO @Announcement []
  pure $ PagedResponse (mapData <$> xs) total

getAnnouncement ::
  Effs '[Error ServerError, Database] m =>
  (SessionId, Entity User) ->
  AnnouncementId ->
  m AnnouncementResponse
getAnnouncement _ k = do
  xs <- fmap groupData . query $
    select $ do
      ((ann, u), group) <- announcementQuery
      where_ (ann ^. AnnouncementId ==. val k)
      return ((ann, u), group)
  maybe (throw err404) (pure . mapData) $ listToMaybe xs

mapData :: (Entity Announcement, Entity User, [Entity AnnouncementGroup]) -> AnnouncementResponse
mapData (a, u, gs) = AnnouncementResponse (entityKey a) (entityVal a) (entityVal u) (entityVal <$> gs)

groupData :: (Ord (Key a)) => [((Entity a, c), Maybe b)] -> [(Entity a, c, [b])]
groupData res =
  reverse . M.elems $ foldr (\((a, c), b) -> M.insertWith mergeData (entityKey a) (a, c, maybeToList b)) M.empty res

mergeData :: (a, c, [b]) -> (a, c, [b]) -> (a, c, [b])
mergeData (a, c, b) (_, _, b') = (a, c, b ++ b')
