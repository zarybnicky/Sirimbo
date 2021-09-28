{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Olymp.API.Reservation
  ( ReservationAPI,
    reservationAPI,
  )
where

import Control.Effect (Effs)
import Control.Effect.Error (Error, throw)
import Database.Persist.Sql (Entity(..), get, selectList, (==.))
import Olymp.Auth (PhpAuth)
import Olymp.Effect.Database (Database, query)
import Olymp.Schema (EntityField (..), Reservation (..), ReservationId, ReservationItem, SessionId, User)
import Servant

type ReservationAPI =
  PhpAuth :> "reservation" :> Capture "id" ReservationId :> Get '[JSON] (User, Reservation, [ReservationItem])

reservationAPI :: Effs '[Error ServerError, Database] m => ServerT ReservationAPI m
reservationAPI = getReservation

getReservation :: Effs '[Error ServerError, Database] m => (SessionId, Entity User) -> ReservationId -> m (User, Reservation, [ReservationItem])
getReservation _ k = do
  reservation <- maybe (throw err404) pure =<< query (get k)
  trainer <- maybe (throw err404) pure =<< query (get $ reservationTrainer reservation)
  items <- query $ selectList [ReservationItemParent ==. k] []
  pure (trainer, reservation, entityVal <$> items)
