{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Olymp.Cli
  ( Command(..)
  , Config(..)
  , parseCli
  ) where

import Barbies (bsequence', bzipWith)
import Data.Functor.Identity (Identity(..))
import Data.Generic.HKD
import Data.Validation (Validation(..), validate)
import Data.Yaml (FromJSON, decodeFileThrow)
import GHC.Generics
import Options.Applicative hiding (Success, Failure)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import System.Exit (exitFailure)
import Data.Monoid (Last)

validateOptions
  :: (TraversableB b, ApplicativeB b)
  => b (Const String)
  -> b Maybe
  -> Validation [String] (b Identity)
validateOptions errs = bsequence' . bzipWith (\(Const e) -> validate ["Please provide " <> e] id) errs

parseCli :: IO (Config, Command)
parseCli = do
  (mCfgFile, cfgEnv) <- decodeEnv
  (mCfgFile', cfgArgs, cmd) <- execParser argsParser
  cfgFile <- case mCfgFile' <|> mCfgFile of
    Nothing -> pure mempty
    Just file -> do
      putStrLn $ "Reading config from: " <> file
      decodeFileThrow file
  case validateOptions label (cfgFile <> cfgEnv <> cfgArgs) of
    Success cfg -> pure (runIdentity (construct cfg), cmd)
    Failure errs -> print errs >> exitFailure

decodeEnv :: IO (Maybe FilePath, HKD Config Maybe)
decodeEnv = (,)
  <$> lookupEnv "CONFIG"
  <*> (build @Config
    <$> lookupEnv "DB_HOST"
    <*> lookupEnv "DB_USER"
    <*> lookupEnv "DB_PASSWORD"
    <*> lookupEnv "DB_DATABASE"
    <*> (maybe Nothing readMaybe <$> lookupEnv "PORT"))

argsParser :: ParserInfo (Maybe FilePath, HKD Config Maybe, Command)
argsParser = info (args <**> helper) fullDesc
  where
    args = (,,) <$> configFile <*> config <*> commands
    configFile = optional $ strOption @FilePath (long "config" <> short 'c' <> metavar "CONFIG_FILE")
    config = build @Config
      <$> (optional $ strOption (long "db-host" <> metavar "HOST"))
      <*> (optional $ strOption (long "db-user" <> metavar "USER"))
      <*> (optional $ strOption (long "db-password" <> metavar "PASSWORD"))
      <*> (optional $ strOption (long "db-database" <> metavar "DATABASE"))
      <*> (optional $ option auto (long "port" <> metavar "PORT"))
    commands = subparser $ mconcat
      [ command "server" (info (pure Server) mempty)
      , command "check-youtube" (info (pure CheckYouTube) mempty)
      ]

data Config = Config
  { dbHost :: String
  , dbUser :: String
  , dbPassword :: String
  , dbDatabase :: String
  , port :: Last Int
  } deriving (Show, Generic)
instance FromJSON (HKD Config Maybe)

data Command
  = Server
  | CheckYouTube
  deriving (Show, Generic)
