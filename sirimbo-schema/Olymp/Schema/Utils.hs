{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Olymp.Schema.Utils
  ( getPrefix
  , mkPersist'
  , persistSchema
  , persistSchemas
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isSuffixOf)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (persistManyFileWith, mkPersist, mpsDeriveInstances, persistFileWith, sqlSettings)
import GHC.Generics (Generic)
import Language.Haskell.TH (Dec, Exp, Q)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Database.Persist (EntityDef)

mkPersist' :: [EntityDef] -> Q [Dec]
mkPersist' = mkPersist sqlSettings { mpsDeriveInstances = [''Show, ''Eq, ''Generic] }

persistSchema :: FilePath -> Q Exp
persistSchema path = do
  prefix <- getPrefix
  persistFileWith lowerCaseSettings (prefix <> path)

persistSchemas :: Q Exp
persistSchemas = do
  prefix <- getPrefix
  files <- liftIO $ getDirectoryContents prefix
  persistManyFileWith lowerCaseSettings $ (prefix <>) <$> filter (".model" ` isSuffixOf`) files

getPrefix :: Q FilePath
getPrefix = liftIO (doesDirectoryExist "sirimbo-schema") >>= \case
    True -> pure "sirimbo-schema/Olymp/Schema/"
    False -> pure "Olymp/Schema/"
