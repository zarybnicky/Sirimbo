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

module Olymp.Schema.User
  ( Key(..)
  , EntityField(..)
  , UserId
  , User(..)
  , UserGroupId
  , UserGroup(..)
  ) where

import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.Persist (Key, EntityField)
import Olymp.Schema.PaymentGroup (PaymentGroupId)
import Olymp.Schema.Utils (mkPersist', persistSchema)

mkPersist' $(persistSchema "User.model")
