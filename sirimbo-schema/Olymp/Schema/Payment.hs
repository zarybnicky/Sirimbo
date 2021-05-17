{-# LANGUAGE DataKinds #-}
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

module Olymp.Schema.Payment
  ( Key(..)
  , Unique(..)
  , EntityField(..)
  , PaymentCategoryId
  , PaymentCategory(..)
  , PaymentCategoryGroupId
  , PaymentCategoryGroup(..)
  , PaymentGroupUserGroupId
  , PaymentGroupUserGroup(..)
  , PaymentItemId
  , PaymentItem(..)
  , PaymentRawId
  , PaymentRaw(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time (Day)
import Database.Persist (Key, Unique, EntityField)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (mkPersist, persistFileWith, share, sqlSettings)
import GHC.Generics (Generic)
import Olymp.Schema.PaymentGroup (PaymentGroupId)
import Olymp.Schema.User (UserGroupId, UserId)
import Olymp.Schema.Orphans ()
import System.Directory (doesDirectoryExist)

share [mkPersist sqlSettings] $(do
  prefix <- liftIO (doesDirectoryExist "sirimbo-schema") >>= \case
    True -> pure "sirimbo-schema/Olymp/Schema"
    False -> pure "Olymp/Schema"
  persistFileWith lowerCaseSettings (prefix <> "/Payment.model"))
