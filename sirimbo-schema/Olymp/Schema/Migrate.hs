{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Olymp.Schema.Migrate
  ( migrateAll
  ) where

import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Database.Persist.TH (mkMigrate)
import Olymp.Schema.Utils (persistSchemas)

mkMigrate "migrateAll" $(persistSchemas)
