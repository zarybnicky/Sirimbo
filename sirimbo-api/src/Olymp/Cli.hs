{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Olymp.Cli
  ( Config (..),
    parseCli,
  )
where

import Barbies (bsequence', bzipWith)
import Control.Lens
import Data.Generic.HKD
import Data.List (intercalate)
import Data.Validation (Validation (..), validate)
import Data.Yaml (decodeFileEither, decodeFileThrow)
import Olymp.Cli.Config (Config (..))
import Olymp.Server (runServer, runCheckYouTube)
import Options.Applicative hiding (Failure, Success)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

validateOptions ::
  (TraversableB b, ApplicativeB b) =>
  b (Const String) ->
  b Maybe ->
  Validation [String] (b Identity)
validateOptions errs = bsequence' . bzipWith (\(Const e) -> validate [e] id) errs

parseCli :: IO (Config, Config -> IO ())
parseCli = do
  (mCfgFile, cfgEnv) <- decodeEnv
  (mCfgFile', cfgArgs, cmd) <- execParser argsParser
  cfgFile <- case mCfgFile' <|> mCfgFile of
    Nothing -> do
      putStrLn "No config specified, trying config.yaml"
      decodeFileEither "config.yaml" >>= \case
        Left _ -> do
          putStrLn "Failed, not using a config file"
          pure mempty
        Right x -> pure x
    Just file -> do
      putStrLn $ "Reading config from: " <> file
      decodeFileThrow file
  case validateOptions label (defaults <> cfgFile <> cfgEnv <> cfgArgs) of
    Success cfg -> pure (runIdentity (construct cfg), cmd)
    Failure errs -> do
      putStrLn ("Missing config options: " <> intercalate ", " errs)
      exitFailure

defaults :: HKD Config Maybe
defaults = build @Config Nothing

decodeEnv :: IO (Maybe FilePath, HKD Config Maybe)
decodeEnv =
  (,)
    <$> lookupEnv "CONFIG"
    <*> ( build @Config
            <$> lookupEnv "DB_CONN_STRING"
        )

argsParser :: ParserInfo (Maybe FilePath, HKD Config Maybe, Config -> IO ())
argsParser = info (args <**> helper) fullDesc
  where
    args = (,,) <$> configFile <*> config <*> commands
    configFile = optional $ strOption @FilePath (long "config" <> short 'c' <> metavar "CONFIG_FILE")
    config =
      build @Config
        <$> optional (strOption (long "db-conn-string" <> metavar "DBCONN"))
    commands =
      subparser $
        mconcat
          [ command
              "server"
              ( info
                  ( runServer
                      <$> (option auto (long "port" <> metavar "PORT") <|> pure 3000)
                      <*> (option auto (long "proxy" <> metavar "PORT") <|> pure 3010)
                  )
                  mempty
              ),
            command "check-youtube" (info (pure runCheckYouTube) mempty)
          ]
