{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Olymp.Cli
  ( Command(..)
  , Config(..)
  , parseCli
  ) where

import Barbies (bsequence', bzipWith)
import Control.Lens
import Data.Generic.HKD
import Data.List (intercalate)
import Data.Validation (Validation(..), validate)
import Data.Yaml (FromJSON, decodeFileThrow)
import GHC.Generics
import Options.Applicative hiding (Success, Failure)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

validateOptions
  :: (TraversableB b, ApplicativeB b)
  => b (Const String)
  -> b Maybe
  -> Validation [String] (b Identity)
validateOptions errs = bsequence' . bzipWith (\(Const e) -> validate [e] id) errs

parseCli :: IO (Config, Command)
parseCli = do
  (mCfgFile, cfgEnv) <- decodeEnv
  (mCfgFile', cfgArgs, cmd) <- execParser argsParser
  cfgFile <- case mCfgFile' <|> mCfgFile of
    Nothing -> do
      putStrLn "No config specified, trying config.yaml"
      decodeFileThrow "config.yaml"
    Just file -> do
      putStrLn $ "Reading config from: " <> file
      decodeFileThrow file
  case validateOptions label (cfgFile <> cfgEnv <> cfgArgs) of
    Success cfg -> pure (runIdentity (construct cfg), cmd)
    Failure errs -> do
      putStrLn ("Missing config options: " <> intercalate ", " errs)
      exitFailure

decodeEnv :: IO (Maybe FilePath, HKD Config Maybe)
decodeEnv = (,)
  <$> lookupEnv "CONFIG"
  <*> (build @Config
    <$> lookupEnv "DB_HOST"
    <*> lookupEnv "DB_USER"
    <*> lookupEnv "DB_PASSWORD"
    <*> lookupEnv "DB_DATABASE")

argsParser :: ParserInfo (Maybe FilePath, HKD Config Maybe, Command)
argsParser = info (args <**> helper) fullDesc
  where
    args = (,,) <$> configFile <*> config <*> commands
    configFile = optional $ strOption @FilePath (long "config" <> short 'c' <> metavar "CONFIG_FILE")
    config = build @Config
      <$> optional (strOption (long "db-host" <> metavar "HOST"))
      <*> optional (strOption (long "db-user" <> metavar "USER"))
      <*> optional (strOption (long "db-password" <> metavar "PASSWORD"))
      <*> optional (strOption (long "db-database" <> metavar "DATABASE"))
    commands = subparser $ mconcat
      [ command "server"
        (info (Server <$> (option auto (long "port" <> metavar "PORT") <|> pure 3000)) mempty)
      , command "check-youtube" (info (pure CheckYouTube) mempty)
      , command "migrate"
        (info (Migrate <$> (flag' True (long "execute") <|> flag' False (long "dry-run"))) mempty)
      ]

data Config = Config
  { dbHost :: String
  , dbUser :: String
  , dbPassword :: String
  , dbDatabase :: String
  } deriving (Show, Generic)
instance FromJSON (HKD Config Maybe)

data Command
  = Server Int
  | CheckYouTube
  | Migrate Bool
  deriving (Show, Generic)
