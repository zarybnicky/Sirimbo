{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Olymp.Schema.Utils
  ( getPrefix,
    mkPersist',
    persistSchema,
    persistSchemas,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (isSuffixOf)
import Data.OpenApi (NamedSchema (..), ToSchema (..), ToParamSchema (..), declareSchema)
import Data.Proxy (Proxy (..))
import Database.Persist (EntityDef, Key, BackendKey)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (mkPersist, mpsDeriveInstances, persistFileWith, persistManyFileWith, sqlSettings)
import GHC.Generics (Generic)
import Language.Haskell.TH (Dec, Exp, Q)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Database.Persist.Sql (SqlBackend)
import Data.Aeson (Value, Object)
import Data.Typeable (Typeable)

instance ToSchema (BackendKey SqlBackend) where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy @Integer)
    pure $ NamedSchema (Just "Key") schema

instance {-# OVERLAPPABLE #-} Typeable a => ToSchema (Key a) where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy @Integer)
    pure $ NamedSchema (Just "Key") schema

instance {-# OVERLAPPABLE #-} Typeable a => ToParamSchema (Key a) where
  toParamSchema _ = toParamSchema (Proxy @Integer)

instance ToSchema Value where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy @Object)
    pure $ NamedSchema Nothing schema

mkPersist' :: [EntityDef] -> Q [Dec]
mkPersist' = mkPersist sqlSettings {mpsDeriveInstances = [''Show, ''Eq, ''Generic, ''ToSchema]}

persistSchema :: FilePath -> Q Exp
persistSchema path = do
  prefix <- getPrefix
  persistFileWith lowerCaseSettings (prefix <> path)

persistSchemas :: Q Exp
persistSchemas = do
  prefix <- getPrefix
  files <- liftIO $ getDirectoryContents prefix
  persistManyFileWith lowerCaseSettings $ (prefix <>) <$> filter (".model" `isSuffixOf`) files

getPrefix :: Q FilePath
getPrefix =
  liftIO (doesDirectoryExist "sirimbo-schema") >>= \case
    True -> pure "sirimbo-schema/Olymp/Schema/"
    False -> pure "Olymp/Schema/"
