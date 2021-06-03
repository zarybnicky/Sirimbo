{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Server
  ( AppStack
  , OlympApi
  , interpretServer
  , olympServer
  ) where

import Control.Effect (Effs, InterpretSimpleC, runM)
import Control.Effect.AtomicState (AtomicState, runAtomicStateIORefSimple)
import Control.Effect.Bracket (bracketToIO, BracketToIOC, Bracket)
import Control.Effect.Embed (Embed, RunMC)
import Control.Effect.Error (ErrorC, Error, runError)
import Data.Csv (Only)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.MySQL (entityVal, Entity, SqlBackend)
import Network.HTTP.Client (Manager)
import Network.HTTP.ReverseProxy (ProxyDest(..), WaiProxyResponse(..), waiProxyTo, defaultOnExc)
import Network.WebSockets (Connection)
import Olymp.API.Payment (QrPaymentAPI, qrPaymentAPI)
import Olymp.Auth (PhpAuth, PhpAuthHandler, phpAuthHandler)
import Olymp.Effect.Database (Database, runDatabasePool)
import Olymp.Effect.Error (AppError, runAppErrorToError)
import Olymp.Effect.Log (Log, WithLog, logInfo, runLogToStdout)
import Olymp.Effect.User (UserEff, runUserEffPersistent)
import Olymp.Effect.Session (deleteSession, SessionEff, runSessionEffPersistent)
import Olymp.Schema (SessionId, User(..))
import Olymp.Tournament.Base (Tournament, NodeId)
import Olymp.Tournament.API (tournamentSocket, tournamentAdminSocket)
import Olymp.WordPress (WordpressApi, wordpressServer)
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.CSV.Cassava (CSV', HasHeader(NoHeader), DefaultOpts)
import Web.Cookie (defaultSetCookie, SetCookie(..))
import Data.Time (Day(ModifiedJulianDay), UTCTime(..))

type AppStack
   = '[ UserEff
      , SessionEff
      , AppError
      , Error ServerError
      , AtomicState (Map Int Connection)
      , AtomicState (Tournament NodeId)
      , Database
      , Bracket
      , Log
      , Embed IO
      ]

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

type OlympApi
  = PhpAuth :> "api" :> "whoami" :> Get '[PlainText, JSON] Text
  :<|> PhpAuth :> "api" :> "export-emails" :> Get '[CSV' 'NoHeader DefaultOpts] [Only Text]
  :<|> QrPaymentAPI
  :<|> "api" :> "tournament" :> "ws" :> WebSocket
  :<|> PhpAuth :> "api" :> "tournament" :> "admin" :> "ws" :> WebSocket
  :<|> PhpAuth :> "logout" :> Verb 'GET 303 '[JSON]
    (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)
  :<|> "wp" :> "v2" :> WordpressApi

server :: Effs AppStack m => ServerT OlympApi m
server
  = whoAmI
  :<|> exportEmails
  :<|> qrPaymentAPI
  :<|> tournamentSocket
  :<|> tournamentAdminSocket
  :<|> logout
  :<|> wordpressServer

logout
  :: Effs '[SessionEff] m
  => (SessionId, Entity User)
  -> m (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)
logout (sid, _) = do
  deleteSession sid
  pure $ addHeader sessionCookie (addHeader "/" NoContent)
  where
    sessionCookie = defaultSetCookie
      { setCookieName = "PHPSESSID"
      , setCookieValue = ""
      , setCookiePath = Just "/"
      , setCookieExpires = Just (UTCTime (ModifiedJulianDay 50000) 0)
      }

whoAmI :: WithLog m => (SessionId, Entity User) -> m Text
whoAmI (_, eu) = do
  let u = entityVal eu
  logInfo (userName u)
  pure (userName u <> " " <> userSurname u)

exportEmails :: WithLog m => (SessionId, Entity User) -> m [Only Text]
exportEmails _ = pure []

type AppC = InterpretSimpleC UserEff
  (InterpretSimpleC SessionEff
   (InterpretSimpleC AppError
    (ErrorC ServerError
     (InterpretSimpleC (AtomicState (Map Int Connection))
      (InterpretSimpleC (AtomicState (Tournament NodeId))
       (InterpretSimpleC Database
        (InterpretSimpleC Log
         (BracketToIOC
          (RunMC IO)))))))))

interpretServer ::
     IORef (Tournament NodeId)
  -> IORef (Map Int Connection)
  -> Pool SqlBackend
  -> AppC a
  -> IO (Either ServerError a)
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
  runUserEffPersistent f
