{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Olymp
  ( makeServer
  , parseArgs
  , runServer
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Lens
import Control.Monad.Except (ExceptT(..), runExceptT, forever, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode')
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import Data.Generics.Product (field)
import Data.IORef (IORef, newIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileThrow)
import Database.Persist.Sql (get, repsert)
import Database.Persist.MySQL (SqlBackend, createMySQLPool, mkMySQLConnectInfo)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets (Connection, forkPingThread, receiveData, sendTextData)
import Olymp.Auth (PhpAuth, PhpAuthHandler, phpAuthHandler)
import Olymp.Cli (Args(..), parseArgs)
import Olymp.Effect.Database (Database, runDatabasePool, query)
import Olymp.Effect.Error (AppError, runAppErrorToError)
import Olymp.Effect.Log (WithLog, Log, logInfo, logError, runLogToStdout)
import Olymp.Effect.User (UserEff, runUserEffPersistent)
import Olymp.Effect.Session (SessionEff, runSessionEffPersistent)
import Olymp.Schema (User(..), Key(ParameterKey), Parameter(..))
import Olymp.Tournament
import Polysemy (Embed, Sem, runM)
import Polysemy.AtomicState
  (AtomicState, runAtomicStateIORef, atomicGet, atomicPut, atomicState', atomicModify')
import Polysemy.Error (Error, runError)
import Polysemy.Resource (Resource, finally, resourceToIO)
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.Server.Internal.Handler (runHandler')
import System.Environment (lookupEnv)

data AppConfig = AppConfig
  { dbHost :: String
  , dbUser :: String
  , dbPassword :: String
  , dbDatabase :: String
  } deriving (Show, Generic, FromJSON)

runServer :: IO ()
runServer = do
  args <- parseArgs
  run (fromMaybe 4000 (port args)) =<< makeServer

initialTournament :: Tournament NodeId
initialTournament = (createTournament ps) { userFocus = Just 2 }
  where
    ps = M.fromList
      [ (0 :: PlayerId, Player "Dan a Barča" "Daniel a Barbora Borůvkovi")
      , (1, Player "Tomáš a Monika" "Tomáš Opichal a Monika Maňásková")
      , (2, Player "Roman a Anička" "Roman Pecha a Anna Banková")
      , (3, Player "Vilda a Hanka" "Vilém Šír a Hana-Anna Šišková")
      ]

makeServer :: IO Application
makeServer = do
  configFile <- fromMaybe "config.yaml" <$> lookupEnv "CONFIG"
  putStrLn $ "Reading config from: " <> configFile
  AppConfig{..} <- decodeFileThrow configFile
  let connectInfo =
        mkMySQLConnectInfo dbHost (BC.pack dbUser) (BC.pack dbPassword) (BC.pack dbDatabase)
  pool <- runStdoutLoggingT $ createMySQLPool connectInfo 5

  t <- withResource pool (runReaderT (get $ ParameterKey "tournament")) >>= \case
    Nothing -> pure initialTournament
    Just (Parameter tt) -> case eitherDecode' (BCL.pack $ T.unpack tt) of
      Left err -> putStrLn err >> pure initialTournament
      Right tt' -> pure tt'
  ref <- newIORef (withTournament propagateWinners t)
  ref' <- newIORef M.empty
  let runner :: forall a. Sem AppStack a -> Handler a
      runner = interpretServer ref ref' pool

  let saveState = runExceptT $ runHandler' $ runner $ do
        liftIO $ threadDelay 1000000
        state <- T.pack . BCL.unpack . encode <$> atomicGet @(Tournament NodeId)
        _ <- query $ repsert (ParameterKey "tournament") (Parameter state)
        pure ()
  _ <- forkIO (forever saveState)

  pure $ appServer runner
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
   = '[ UserEff
      , SessionEff
      , AppError
      , Error ServerError
      , AtomicState (Map Int Connection)
      , AtomicState (Tournament NodeId)
      , Database SqlBackend
      , Resource
      , Log
      , Embed IO
      ]

interpretServer ::
     IORef (Tournament NodeId)
  -> IORef (Map Int Connection)
  -> Pool SqlBackend
  -> Sem AppStack a
  -> Handler a
interpretServer ref ref' pool =
  Handler . ExceptT . runM .
  runLogToStdout .
  resourceToIO .
  runDatabasePool pool .
  runAtomicStateIORef ref .
  runAtomicStateIORef ref' .
  runError @ServerError .
  runAppErrorToError .
  runSessionEffPersistent .
  runUserEffPersistent

type OlympApi
  = PhpAuth :> "whoami" :> Get '[PlainText] Text
  :<|> "tournament" :> "ws" :> WebSocket
  :<|> PhpAuth :> "tournament" :> "admin" :> "ws" :> WebSocket

server :: ServerT OlympApi (Sem AppStack)
server = whoAmI :<|> tournamentSocket :<|> tournamentAdminSocket

whoAmI :: WithLog r => User -> Sem r Text
whoAmI u = do
  logInfo (userName u)
  pure (userName u <> " " <> userSurname u)

data TournamentUser =
  Vote NodeId Int Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data TournamentBroadcast
  = StateMsg (Tournament NodeId)
  | ResetAll
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data TournamentAdmin
  = UpdatePlayer PlayerId Player
  | OpenVoting NodeId
  | CloseVoting NodeId
  | FocusNode (Maybe NodeId)
  | UserFocusNode (Maybe NodeId)
  | ResetScore NodeId
  | ResetState
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- persistent entities
-- player CRUD

withSocketLoop :: FromJSON a => Connection -> (a -> Sem AppStack ()) -> Sem AppStack ()
withSocketLoop c f = do
  welcomeMsg <- encode . StateMsg <$> atomicGet
  liftIO $ forkPingThread c 10 >> sendTextData c welcomeMsg
  k <- atomicState' (\m -> let k = 1 + foldr max 0 (M.keys m) in (M.insert k c m, k))
  flip finally (atomicModify' $ M.delete k) $ forever $
    eitherDecode' <$> liftIO (receiveData c) >>= either (logError . T.pack) f

updateNode ::
     NodeId
  -> (TournamentNodeF NodeId NodeId -> Maybe (TournamentNodeF NodeId NodeId))
  -> Text
  -> Sem AppStack ()
updateNode nid f msg = do
  mErr <- atomicState' @(Tournament NodeId) $ \t ->
    case M.lookup nid (nodes t) of
      Nothing -> (t, Just $ "Unknown node in " <> msg)
      Just n -> case f n of
        Nothing -> (t, Just $ "Node not ready for " <> msg)
        Just new -> (t & field @"nodes" . ix nid .~ new, Nothing)
  case mErr of
    Nothing -> broadcastState
    Just err -> logError err

tournamentSocket :: Connection -> Sem AppStack ()
tournamentSocket c = withSocketLoop c $ \msg -> case msg of
  Vote nid left right ->
    updateNode nid (setScore (+ left) (+ right)) (tshow msg)

tournamentAdminSocket :: User -> Connection -> Sem AppStack ()
tournamentAdminSocket _ c = withSocketLoop c $ \msg -> case msg of
  UpdatePlayer pid p -> do
    atomicModify' @(Tournament NodeId) (field @"tournamentPlayers" %~ M.insert pid p)
    broadcastState
  FocusNode nid -> do
    atomicModify' @(Tournament NodeId) (field @"dashboardFocus" .~ nid)
    broadcastState
  UserFocusNode n -> do
    atomicModify' @(Tournament NodeId) (field @"userFocus" .~ n)
    broadcastState
  OpenVoting nid -> updateNode nid openVoting (tshow msg)
  ResetScore nid -> updateNode nid (setScore (const 0) (const 0)) (tshow msg)
  ResetState -> do
    state <- atomicGet @(Tournament NodeId)
    let initial = withTournament propagateWinners initialTournament
    atomicPut @(Tournament NodeId) initial { tournamentPlayers = tournamentPlayers state }
    conns <- atomicGet @(Map Int Connection)
    liftIO $ mapM_ (`sendTextData` encode ResetAll) conns
    broadcastState
  CloseVoting nid -> do
    mErr <- atomicState' @(Tournament NodeId) $ \t ->
      case M.lookup nid (nodes t) of
        Nothing -> (t, Just $ "Unknown node in " <> tshow msg)
        Just n -> case closeVoting n of
          Nothing -> (t, Just $ "Node not ready for " <> tshow msg)
          Just closed ->
            let nodes' = nodes t & ix nid .~ closed
                loser = either (const 0) id $ fromMaybe 0 . getLoser <$> treeify nodes' (nodeId closed)
                lb = maybe M.empty
                      (withTree (propagateWinners . snd . fillPlayers [loser]) nodes')
                      (losersRoot t)
                wb = withTree propagateWinners nodes' (winnersRoot t)
            in (t & field @"nodes" .~ wb <> lb, Nothing)
    case mErr of
      Nothing -> broadcastState
      Just err -> logError err

broadcastState :: Sem AppStack ()
broadcastState = do
  msg <- encode . StateMsg <$> atomicGet @(Tournament NodeId)
  conns <- atomicGet @(Map Int Connection)
  liftIO $ mapM_ (`sendTextData` msg) conns
  pure ()

tshow :: Show a => a -> Text
tshow = T.pack . show
