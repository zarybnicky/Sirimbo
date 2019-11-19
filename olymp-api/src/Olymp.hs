{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Olymp
  ( makeApplication
  , parseArgs
  , Warp.run
  ) where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Yaml (decodeFileThrow)
import Database.Persist.Sql ()
import Database.Persist.MySQL (SqlBackend, createMySQLPool, mkMySQLConnectInfo)
import GHC.Generics (Generic)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Olymp.Auth (PhpAuth, PhpAuthHandler, phpAuthHandler)
import Olymp.Cli (Args(..), parseArgs)
import Olymp.Effect.Database (Database, runDatabasePool)
import Olymp.Effect.Error (AppError, runAppErrorToError)
import Olymp.Effect.Log (WithLog, Log, logInfo, runLogToStdout)
import Olymp.Effect.User (UserEff, runUserEffPersistent)
import Olymp.Effect.Session (SessionEff, runSessionEffPersistent)
import Olymp.Schema (User(..))
import Polysemy (Embed, Sem, runM)
import Polysemy.Error (Error, runError)
import Polysemy.Resource (Resource, resourceToIO)
import Servant
  ((:>), Context(..), Get, Handler(..), PlainText, ServerError, ServerT,
   hoistServerWithContext, serveWithContext)
import System.Environment (lookupEnv)

data AppConfig = AppConfig
  { dbHost :: String
  , dbUser :: String
  , dbPassword :: String
  , dbDatabase :: String
  } deriving (Show, Generic, FromJSON)

makeApplication :: Args -> IO (Int, Application)
makeApplication args = do
  configFile <- fromMaybe "config.yaml" <$> lookupEnv "CONFIG"
  putStrLn $ "Reading config from: " <> configFile
  AppConfig{..} <- decodeFileThrow configFile
  let connectInfo =
        mkMySQLConnectInfo dbHost (BC.pack dbUser) (BC.pack dbPassword) (BC.pack dbDatabase)

  pool <- runStdoutLoggingT $ createMySQLPool connectInfo 5
  pure (port args, appServer (interpretServer pool))
  where
    appServer :: (forall a. Sem AppStack a -> Handler a) -> Application
    appServer runner =
      serveWithContext (Proxy @OlympApi) (phpAuthHandler runner :. EmptyContext) $
      hoistServerWithContext
        (Proxy @OlympApi)
        (Proxy @'[PhpAuthHandler])
        runner
        server

type AppStack
   = '[ UserEff, SessionEff, AppError, Error ServerError, Database SqlBackend, Resource, Log, Embed IO]

interpretServer :: Pool SqlBackend -> Sem AppStack a -> Handler a
interpretServer pool =
  Handler . ExceptT . runM .
  runLogToStdout .
  resourceToIO .
  runDatabasePool pool .
  runError @ServerError .
  runAppErrorToError .
  runSessionEffPersistent .
  runUserEffPersistent

type OlympApi = PhpAuth :> "whoami" :> Get '[PlainText] Text

server :: WithLog r => ServerT OlympApi (Sem r)
server u = logInfo (userName u) >> pure (userName u <> " " <> userSurname u)
