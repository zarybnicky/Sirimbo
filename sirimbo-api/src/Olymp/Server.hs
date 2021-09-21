{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
    OlympApi,
    interpretServer,
    olympServer,
    generateTs,
  )
where

import Control.Effect (CompositionC, Effs, InterpretSimpleC, runComposition, runM)
import Control.Effect.AtomicState (AtomicState, runAtomicStateIORefSimple)
import Control.Effect.Bracket (Bracket, BracketToIOC, bracketToIO)
import Control.Effect.Embed (Embed, RunMC)
import Control.Effect.Error (Error, ErrorC, runError, throw)
import Data.Aeson (ToJSON)
import Data.Csv (Only (..))
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time (Day (ModifiedJulianDay), UTCTime (..))
import Database.Persist.MySQL (Entity, PersistStoreRead (get), SqlBackend, entityVal)
import Network.HTTP.Client (Manager)
import Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..), defaultOnExc, waiProxyTo)
import Network.WebSockets (Connection)
import Olymp.API.Payment (QrPaymentAPI, qrPaymentAPI)
import Olymp.Auth (PhpAuth, PhpAuthHandler, phpAuthHandler)
import Olymp.Effect.Database (Database, query, runDatabasePool)
import Olymp.Effect.Error (AppError, runAppErrorToError)
import Olymp.Effect.Log (Log, WithLog, logInfo, runLogToStdout)
import Olymp.Effect.Session (SessionEff, deleteSession, runSessionEffPersistent)
import Olymp.Effect.User (UserEff, runUserEffPersistent)
import Olymp.Schema (Reservation, ReservationId, ReservationItem, SessionId, User (..))
import Olymp.Tournament.API (tournamentAdminSocket, tournamentSocket)
import Olymp.Tournament.Base (NodeId, Tournament)
import Olymp.WordPress (WordpressApi, wordpressServer)
import Servant
import Servant.API.Flatten (Flat)
import Servant.API.WebSocket (WebSocket)
import Servant.CSV.Cassava (CSV', DefaultOpts, HasHeader (NoHeader))
import Servant.TypeScript (FilterAPI, apiToTypeScript)
import Web.Cookie (SetCookie (..), defaultSetCookie)

type AppStack =
  '[ UserEff,
     SessionEff,
     AppError,
     Error ServerError,
     AtomicState (Map Int Connection),
     AtomicState (Tournament NodeId),
     Database,
     Bracket,
     Log,
     Embed IO
   ]

deriving newtype instance ToJSON a => ToJSON (Only a)

olympServer :: (Effs AppStack m) => Int -> Manager -> (forall a. m a -> Handler a) -> Application
olympServer proxyPort manager runner =
  -- cors (const $ Just simpleCorsResourcePolicy
  --       { corsRequestHeaders = ["Content-Type"]
  --       , corsMethods = "PUT" : simpleMethods
  --       })
  serveWithContext (Proxy @(OlympApi :<|> Raw)) (phpAuthHandler runner :. EmptyContext) $
    api :<|> Tagged phpProxy
  where
    api = hoistServerWithContext (Proxy @OlympApi) (Proxy @'[PhpAuthHandler]) runner server
    phpProxy = waiProxyTo forwardRequest defaultOnExc manager
    forwardRequest _ = pure $ WPRProxyDest (ProxyDest "127.0.0.1" proxyPort)

generateTs :: IO ()
generateTs = T.putStrLn (apiToTypeScript (Proxy @(FilterAPI (Flat OlympApi))))

type OlympApi =
  PhpAuth :> "api" :> "whoami" :> Get '[PlainText, JSON] Text
    :<|> PhpAuth :> "api" :> "export-emails" :> Get '[JSON, CSV' 'NoHeader DefaultOpts] [Only Text]
    :<|> PhpAuth :> "api" :> "reservation" :> Capture "id" ReservationId :> Get '[JSON] (User, Reservation, [ReservationItem])
    :<|> QrPaymentAPI
    :<|> "api" :> "tournament" :> "ws" :> WebSocket
    :<|> PhpAuth :> "api" :> "tournament" :> "admin" :> "ws" :> WebSocket
    :<|> PhpAuth :> "logout"
      :> Verb
           'GET
           303
           '[JSON]
           (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)
    :<|> "wp" :> "v2" :> WordpressApi

server :: Effs AppStack m => ServerT OlympApi m
server =
  whoAmI
    :<|> exportEmails
    :<|> getReservation
    :<|> qrPaymentAPI
    :<|> tournamentSocket
    :<|> tournamentAdminSocket
    :<|> logout
    :<|> wordpressServer

getReservation :: Effs '[Error ServerError, Database] m => (SessionId, Entity User) -> ReservationId -> m (User, Reservation, [ReservationItem])
getReservation _ k = do
  res <- query $
    get k >>= \case
      Nothing -> pure Nothing
      Just r -> pure $ Just (undefined, r, [])
-- get (reservationTrainer r) >>= \case
--   Nothing -> pure Nothing
--   Just u -> do
--     ri <- selectList [ReservationItemParent _] []
--     pure $ Just (entityVal <$> u, r, entityVal <$> ri)
  case res of
    Nothing -> throw err404
    Just r -> pure r

logout ::
  Effs '[SessionEff] m =>
  (SessionId, Entity User) ->
  m (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)
logout (sid, _) = do
  deleteSession sid
  pure $ addHeader sessionCookie (addHeader "/" NoContent)
  where
    sessionCookie =
      defaultSetCookie
        { setCookieName = "PHPSESSID",
          setCookieValue = "",
          setCookiePath = Just "/",
          setCookieExpires = Just (UTCTime (ModifiedJulianDay 50000) 0)
        }

whoAmI :: WithLog m => (SessionId, Entity User) -> m Text
whoAmI (_, eu) = do
  let u = entityVal eu
  logInfo (userName u)
  pure (userName u <> " " <> userSurname u)

exportEmails :: WithLog m => (SessionId, Entity User) -> m [Only Text]
exportEmails _ = pure mempty

type AppC =
  CompositionC
    '[ InterpretSimpleC UserEff,
       InterpretSimpleC SessionEff,
       InterpretSimpleC AppError,
       ErrorC ServerError,
       InterpretSimpleC (AtomicState (Map Int Connection)),
       InterpretSimpleC (AtomicState (Tournament NodeId)),
       InterpretSimpleC Database,
       InterpretSimpleC Log,
       BracketToIOC
     ]

interpretServer ::
  IORef (Tournament NodeId) ->
  IORef (Map Int Connection) ->
  Pool SqlBackend ->
  AppC (RunMC IO) a ->
  IO (Either ServerError a)
interpretServer ref ref' pool f =
  runM $
    bracketToIO $
      runLogToStdout $
        runDatabasePool pool $
          runAtomicStateIORefSimple ref $
            runAtomicStateIORefSimple ref' $
              runError @ServerError $
                runAppErrorToError $
                  runSessionEffPersistent $
                    runUserEffPersistent $
                      runComposition f
