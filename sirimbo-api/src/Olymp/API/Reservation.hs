{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Database.Esqueleto (InnerJoin(..), select, from, on, where_, (^.), concat_, (==.), val, unValue)
import Database.Persist.Sql (Entity (..), get)
import GHC.Generics (Generic)
import Olymp.Auth (PhpAuth)
import Olymp.Effect.Database (Database, query)
import Olymp.Schema (EntityField (..), Reservation (..), ReservationId, SessionId, User)
import Servant

type ReservationAPI =
  PhpAuth :> "reservation" :> Capture "id" ReservationId :> Get '[JSON] ReservationResponse

data ReservationResponse = ReservationResponse
  { trainer :: User,
    reservation :: Reservation,
    items :: [(Text, Int)]
  }
  deriving (Generic, ToJSON, ToSchema)

reservationAPI :: Effs '[Error ServerError, Database] m => ServerT ReservationAPI m
reservationAPI = getReservation

getReservation :: Effs '[Error ServerError, Database] m => (SessionId, Entity User) -> ReservationId -> m ReservationResponse
getReservation _ k = do
  reservation <- maybe (throw err404) pure =<< query (get k)
  trainer <- maybe (throw err404) pure =<< query (get $ reservationTrainer reservation)
  items <- query $ select $ from $ \(ri `InnerJoin` c `InnerJoin` u) -> do
    on (ri ^. ReservationItemPartner ==. c ^. CoupleId)
    on (c ^. CouplePartnerLeader ==. u ^. UserId)
    where_ (ri ^. ReservationItemParent ==. val k)
    pure (concat_ [u ^. UserName, val " ", u ^. UserSurname], ri ^. ReservationItemNumberLessons)
  pure $ ReservationResponse trainer reservation (bimap unValue unValue <$> items)
