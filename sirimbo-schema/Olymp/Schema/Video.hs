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

module Olymp.Schema.Video
  ( Key(..)
  , EntityField(..)
  , VideoId
  , Video(..)
  , VideoListId
  , VideoList(..)
  , VideoSourceId
  , VideoSource(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Key, EntityField)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (mkPersist, persistFileWith, sqlSettings)
import System.Directory (doesDirectoryExist)

mkPersist sqlSettings $(do
  prefix <- liftIO (doesDirectoryExist "sirimbo-schema") >>= \case
    True -> pure "sirimbo-schema/Olymp/Schema"
    False -> pure "Olymp/Schema"
  persistFileWith lowerCaseSettings (prefix <> "/Video.model"))
