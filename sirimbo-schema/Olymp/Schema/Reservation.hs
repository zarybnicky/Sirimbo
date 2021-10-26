{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Olymp.Schema.Reservation
  ( Key(..)
  , Unique(..)
  , EntityField(..)
  , ReservationId
  , Reservation(..)
  , ReservationItemId
  , ReservationItem(..)
  ) where

import Data.Time (Day, UTCTime)
import Database.Persist (Key, Unique, EntityField)
import Olymp.Schema.Couple (CoupleId)
import Olymp.Schema.User (UserId)
import Olymp.Schema.Utils (mkPersist', persistSchema)

mkPersist' $(persistSchema "Reservation.model")
