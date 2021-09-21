{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Olymp.Schema.Migrate
  ( migrateAll
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (mkMigrate, persistManyFileWith)
import System.Directory (doesDirectoryExist, getDirectoryContents)

mkMigrate "migrateAll" $(do
  prefix <- liftIO (doesDirectoryExist "sirimbo-schema") >>= \case
    True -> pure "sirimbo-schema/Olymp/Schema/"
    False -> pure "Olymp/Schema/"
  files <- liftIO (filter (".model" `isSuffixOf`) <$> getDirectoryContents prefix)
  persistManyFileWith lowerCaseSettings $ (prefix <>) <$> files)
