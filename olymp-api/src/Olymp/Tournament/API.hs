{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Olymp.Tournament.API
  ( tournamentSocket
  , tournamentAdminSocket
  , initialTournament
  ) where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode')
import Data.Generics.Product (field)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.WebSockets (Connection, forkPingThread, receiveData, sendTextData)
import Olymp.Effect.Log (logError)
import Olymp.Monad (AppStack)
import Olymp.Schema (User)
import Olymp.Tournament
import Polysemy (Sem)
import Polysemy.AtomicState (atomicGet, atomicModify', atomicPut, atomicState')
import Polysemy.Resource (finally)

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

initialTournament :: Tournament NodeId
initialTournament = (createTournament ps) { userFocus = Just 2 }
  where
    ps = M.fromList
      [ (0 :: PlayerId, Player "Dan a Barča" "Daniel a Barbora Borůvkovi")
      , (1, Player "Tomáš a Monika" "Tomáš Opichal a Monika Maňásková")
      , (2, Player "Roman a Anička" "Roman Pecha a Anna Banková")
      , (3, Player "Vilda a Hanka" "Vilém Šír a Hana-Anna Šišková")
      ]

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
