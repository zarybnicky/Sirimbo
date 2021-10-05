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

import Database.Esqueleto (InnerJoin (..), concat_, from, on, select, unValue, val, where_, (==.), (^.))
import Olymp.Prelude
import Olymp.Schema (Reservation (..), ReservationId)

type ReservationAPI =
  PhpAuth :> Capture "id" ReservationId
    :> OpId "getReservation"
    :> Get '[JSON] ReservationResponse
    :<|> PhpAuth :> Capture "id" ReservationId
      :> OpId "toggleVisibleReservation"
      :> "toggle-visible"
      :> Get '[JSON] Bool

data ReservationItemResponse = ReservationItemResponse
  { person :: Text,
    count :: Int
  }
  deriving (Generic, ToJSON, ToSchema)

data ReservationResponse = ReservationResponse
  { trainer :: User,
    reservation :: Reservation,
    items :: [ReservationItemResponse]
  }
  deriving (Generic, ToJSON, ToSchema)

reservationAPI :: Effs '[Error ServerError, Database] m => ServerT ReservationAPI m
reservationAPI = getReservation :<|> toggleVisible

getReservation :: Effs '[Error ServerError, Database] m => (SessionId, Entity User) -> ReservationId -> m ReservationResponse
getReservation _ k = do
  reservation <- maybe (throw err404) pure =<< query (get k)
  trainer <- maybe (throw err404) pure =<< query (get $ reservationTrainer reservation)
  items <- query $
    select $
      from $ \(ri `InnerJoin` c `InnerJoin` u) -> do
        on (ri ^. ReservationItemPartner ==. c ^. CoupleId)
        on (c ^. CouplePartnerLeader ==. u ^. UserId)
        where_ (ri ^. ReservationItemParent ==. val k)
        pure (concat_ [u ^. UserName, val " ", u ^. UserSurname], ri ^. ReservationItemNumberLessons)
  let items' = uncurry ReservationItemResponse . bimap unValue unValue <$> items
  pure $ ReservationResponse trainer reservation items'

toggleVisible ::
  Effs '[Error ServerError, Database] m =>
  (SessionId, Entity User) ->
  ReservationId ->
  m Bool
toggleVisible _ k = do
  reservation <- maybe (throw err404) pure =<< query (get k)
  let notVisible = boolToText . not . textToBool $ reservationVisible reservation
  newReservation <- query $ updateGet k [ReservationVisible =. notVisible]
  pure . textToBool $ reservationVisible newReservation
