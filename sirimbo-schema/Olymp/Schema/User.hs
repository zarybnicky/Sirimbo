{-# LANGUAGE DataKinds #-}
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

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.Persist (Key, EntityField)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (mkPersist, persistFileWith, sqlSettings)
import Olymp.Schema.PaymentGroup (PaymentGroupId)
import System.Directory (doesDirectoryExist)

mkPersist sqlSettings $(do
  prefix <- liftIO (doesDirectoryExist "sirimbo-schema") >>= \case
    True -> pure "sirimbo-schema/Olymp/Schema"
    False -> pure "Olymp/Schema"
  persistFileWith lowerCaseSettings (prefix <> "/User.model"))
