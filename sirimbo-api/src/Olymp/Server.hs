{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Olymp.Server
  ( AppStack,
    OlympAPI,
    interpretServer,
    olympServer,
    runServer,
    runCheckYouTube,
    saveTournamentState,
  )
where

-- import Servant.Multipart (MultipartData, MultipartForm, Tmp)
-- import Network.Wai.Middleware.Cors

import Codec.Picture (Image)
import Control.Concurrent (threadDelay)
import Control.Effect (Effs, Embed, embed, runM)
import Control.Effect.AtomicState (AtomicState, atomicGet)
import Control.Effect.Bracket (bracketToIO)
import Control.Effect.Embed (embedToMonadIO)
import Control.Lens
import Control.Monad.Except (ExceptT (..), void)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (eitherDecode', encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import Data.Csv (Only)
import Data.IORef (newIORef)
import qualified Data.Map as M
import Data.OpenApi (NamedSchema (..), OpenApi, ToParamSchema, ToSchema (..), binarySchema)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (SqlBackend, repsert, get)
import Network.Google (LogLevel (Info), envScopes, newEnvWith, newLogger, newManager, tlsManagerSettings)
import Network.Google.Auth (getApplicationDefault)
import Network.Google.YouTube (youTubeReadOnlyScope)
import Network.HTTP.Client (Manager, defaultManagerSettings)
import Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..), defaultOnExc, waiProxyTo)
import qualified Network.Wai.Handler.Warp as Warp
import Olymp.API (OlympAPI, olympAPI)
import Olymp.Auth (PhpAuth, PhpAuthHandler, phpAuthHandler, phpMaybeAuthHandler, PhpMaybeAuthHandler, PhpMaybeAuth)
import Olymp.Cli.Config (Config (..))
import Olymp.Effect (AppStack, interpretServer)
import Olymp.Effect.Database (Database, query, runDatabasePool)
import Olymp.Effect.Google (googleToResourceTIO)
import Olymp.Schema (Key (ParameterKey), Parameter (..))
import Olymp.Tournament.API (initialTournament)
import Olymp.Tournament.Base (NodeId, Tournament, propagateWinners, withTournament)
import Olymp.YouTube.Worker (youtubeWorker)
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.OpenApi (HasOpenApi (..))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

runCheckYouTube :: Config -> IO ()
runCheckYouTube config = do
  lgr <- newLogger Network.Google.Info stdout
  mgr <- newManager tlsManagerSettings
  cred <- getApplicationDefault mgr
  env <- newEnvWith cred lgr mgr <&> (envScopes .~ youTubeReadOnlyScope)
  pool <- makePool config

  runResourceT . runM . embedToMonadIO . bracketToIO . runDatabasePool pool . googleToResourceTIO env $
    youtubeWorker

saveTournamentState :: Effs '[AtomicState (Tournament NodeId), Database, Embed IO] m => m ()
saveTournamentState = do
  embed $ threadDelay 100000000
  state <- T.pack . BCL.unpack . encode <$> atomicGet @(Tournament NodeId)
  void . query $ repsert (ParameterKey "tournament") (Parameter state)

runServer :: Int -> Int -> Config -> IO ()
runServer port proxy config = do
  pool <- makePool config
  t <-
    withResource pool (runReaderT . get $ ParameterKey "tournament") >>= \case
      Nothing -> pure initialTournament
      Just (Parameter tt) -> case eitherDecode' (BCL.pack $ T.unpack tt) of
        Left err -> putStrLn err >> pure initialTournament
        Right tt' -> pure tt'

  mgr <- newManager defaultManagerSettings
  ref <- newIORef (withTournament propagateWinners t)
  ref' <- newIORef M.empty

  -- _ <- forkIO . forever . runM . runDatabasePool pool . runAtomicStateIORefSimple ref $
  --   saveTournamentState
  hSetBuffering stdout LineBuffering

  putStrLn ("Starting server on port " <> show port <> " with proxy on port " <> show proxy)
  Warp.run port $ olympServer proxy mgr (Handler . ExceptT . interpretServer ref ref' pool)

makePool :: Config -> IO (Pool SqlBackend)
makePool config = runStdoutLoggingT $ createPostgresqlPool (BC.pack $ dbConnString config) 5

instance HasOpenApi a => HasOpenApi (PhpAuth :> a) where
  toOpenApi _ = toOpenApi @a Proxy

instance HasOpenApi a => HasOpenApi (PhpMaybeAuth :> a) where
  toOpenApi _ = toOpenApi @a Proxy

instance ToParamSchema f => ToParamSchema (Tagged a f)

instance (Typeable a, ToSchema f) => ToSchema (Tagged a f)

instance ToSchema a => ToSchema (Only a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

instance Typeable a => ToSchema (Image a) where
  declareNamedSchema _ = pure $ NamedSchema (Just "Image") binarySchema

instance HasOpenApi WebSocket where
  toOpenApi _ = mempty

swaggerAPI :: OpenApi
swaggerAPI = toOpenApi (Proxy @OlympAPI)

olympServer :: Effs AppStack m => Int -> Manager -> (forall a. m a -> Handler a) -> Application
olympServer proxyPort manager runner =
  -- cors (const $ Just simpleCorsResourcePolicy
  --       { corsRequestHeaders = ["Content-Type"]
  --       , corsMethods = "PUT" : simpleMethods
  --       })
  serveWithContext (Proxy @(OlympAPI :<|> SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Raw)) (phpMaybeAuthHandler runner :. phpAuthHandler runner :. EmptyContext) $
    api :<|> swaggerSchemaUIServer swaggerAPI :<|> Tagged phpProxy
  where
    api = hoistServerWithContext (Proxy @OlympAPI) (Proxy @'[PhpMaybeAuthHandler, PhpAuthHandler]) runner olympAPI
    phpProxy = waiProxyTo forwardRequest defaultOnExc manager
    forwardRequest _ = pure $ WPRProxyDest (ProxyDest "127.0.0.1" proxyPort)
