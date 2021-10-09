{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Olymp.Prelude
import Olymp.Schema (Announcement (..), AnnouncementId, AnnouncementGroup (..))
import Database.Persist.Sql ((==.))

type AnnouncementAPI =
  PhpAuth :> Capture "id" AnnouncementId
    :> OpId "getAnnouncement"
    :> Get '[JSON] AnnouncementResponse

data AnnouncementResponse = AnnouncementResponse
  { announcement :: Announcement,
    groups :: [Text]
  }
  deriving (Generic, ToJSON, ToSchema)

announcementAPI :: Effs '[Error ServerError, Database] m => ServerT AnnouncementAPI m
announcementAPI = getAnnouncement

getAnnouncement :: Effs '[Error ServerError, Database] m => (SessionId, Entity User) -> AnnouncementId -> m AnnouncementResponse
getAnnouncement _ k = do
  announcement <- maybe (throw err404) pure =<< query (get k)
  groups <- query $ selectList [AnnouncementGroupParent ==. k] []
  pure $ AnnouncementResponse announcement (announcementGroupColor . entityVal <$> groups)
