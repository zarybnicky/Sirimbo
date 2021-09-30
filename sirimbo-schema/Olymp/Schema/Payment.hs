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

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time (Day)
import Database.Persist (Key, Unique, EntityField)
import Olymp.Schema.PaymentGroup (PaymentGroupId)
import Olymp.Schema.User (UserGroupId, UserId)
import Olymp.Schema.Orphans ()
import Olymp.Schema.Utils (mkPersist', persistSchema)

mkPersist' $(persistSchema "Payment.model")
