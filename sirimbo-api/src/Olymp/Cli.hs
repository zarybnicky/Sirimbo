{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Olymp.Cli
  ( Args(..)
  , parseArgs
  ) where

import Options.Generic

parseArgs :: IO Args
parseArgs = getRecord "Server"

data Args = Args
  { port :: Maybe Int
  } deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
    { shortNameModifier = \case
        "port" -> Just 'p'
        _ -> Nothing
    }
