{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Olymp.Cli.Config (Config (..)) where

import Data.Generic.HKD (HKD)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data Config = Config
  { dbHost :: String,
    dbUser :: String,
    dbPassword :: Maybe String,
    dbDatabase :: String
  }
  deriving (Show, Generic)

instance FromJSON (HKD Config Maybe)
