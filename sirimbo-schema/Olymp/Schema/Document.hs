{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Olymp.Schema.Document
  ( Key(..)
  , Unique(..)
  , EntityField(..)
  , DocumentId
  , Document(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Key, Unique, EntityField)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (mkPersist, persistFileWith, share, sqlSettings)
import Olymp.Schema.User (UserId)
import System.Directory (doesDirectoryExist)

share [mkPersist sqlSettings] $(do
  prefix <- liftIO (doesDirectoryExist "sirimbo-schema") >>= \case
    True -> pure "sirimbo-schema/Olymp/Schema"
    False -> pure "Olymp/Schema"
  persistFileWith lowerCaseSettings (prefix <> "/Document.model"))