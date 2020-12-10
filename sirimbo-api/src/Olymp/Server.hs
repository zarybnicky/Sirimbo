{-# LANGUAGE DataKinds #-}
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
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.MySQL (SqlBackend)
import Network.WebSockets (Connection)
import Olymp.Auth (PhpAuth, PhpAuthHandler, phpAuthHandler)
import Olymp.Effect.Database (Database, runDatabasePool)
import Olymp.Effect.Error (AppError, runAppErrorToError)
import Olymp.Effect.Log (Log, WithLog, logInfo, runLogToStdout)
import Olymp.Effect.User (UserEff, runUserEffPersistent)
import Olymp.Effect.Session (SessionEff, runSessionEffPersistent)
import Olymp.Schema (User(..))
import Olymp.Tournament.Base (Tournament, NodeId)
import Olymp.Tournament.API (tournamentSocket, tournamentAdminSocket)
import Olymp.WordPress (WordpressApi, wordpressServer)
import Servant
import Servant.API.WebSocket (WebSocket)

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

olympServer :: Effs AppStack m => (forall a. m a -> Handler a) -> Application
olympServer runner =
  -- cors (const $ Just simpleCorsResourcePolicy
  --       { corsRequestHeaders = ["Content-Type"]
  --       , corsMethods = "PUT" : simpleMethods
  --       }) $
  serveWithContext (Proxy @OlympApi) (phpAuthHandler runner :. EmptyContext) $
  hoistServerWithContext
    (Proxy @OlympApi)
    (Proxy @'[PhpAuthHandler])
    runner
    server

type OlympApi
  = "api" :> "whoami" :> PhpAuth :> Get '[PlainText, JSON] Text
  :<|> "api" :> "tournament" :> "ws" :> WebSocket
  :<|> "api" :> "tournament" :> PhpAuth :> "admin" :> "ws" :> WebSocket
  -- :<|> "api" :> "editor" :> EditorApi
  :<|> "wp" :> "v2" :> WordpressApi

server :: Effs AppStack m => ServerT OlympApi m
server
  = whoAmI
  :<|> tournamentSocket
  :<|> tournamentAdminSocket
  :<|> wordpressServer

whoAmI :: WithLog m => User -> m Text
whoAmI u = do
  logInfo (userName u)
  pure (userName u <> " " <> userSurname u)


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
