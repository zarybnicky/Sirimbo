{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Olymp.API.Event
  ( EventAPI,
    eventAPI,
  )
where

import Control.Effect (Effs)
import Control.Effect.Error (Error, throw)
import Data.Text (Text)
import Database.Persist.Sql (Entity (..), (=.), get, updateGet)
import Olymp.Auth (PhpAuth)
import Olymp.Effect.Database (Database, query)
import Olymp.Schema (EntityField (..), Event (..), EventId, SessionId, User)
import Servant

type EventAPI =
  PhpAuth :> "event" :> Capture "id" EventId :> "toggle-visible" :> Get '[JSON] Bool

eventAPI :: Effs '[Error ServerError, Database] m => ServerT EventAPI m
eventAPI = toggleVisible

toggleVisible :: Effs '[Error ServerError, Database] m => (SessionId, Entity User) -> EventId -> m Bool
toggleVisible _ k = do
  event <- maybe (throw err404) pure =<< query (get k)
  let notVisible = not $ eventVisible event
  newEvent <- query $ updateGet k [EventVisible =. notVisible]
  pure $ eventVisible newEvent
