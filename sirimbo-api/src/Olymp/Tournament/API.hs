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

import Control.Effect (Effs, Embed, embed)
import Control.Effect.AtomicState (AtomicState, atomicGet, atomicModify', atomicPut, atomicState')
import Control.Effect.Bracket (Bracket, finally)
import Control.Lens
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode')
import Data.Either (fromRight)
import Data.Generics.Product (field)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (Entity)
import GHC.Generics (Generic)
import Network.WebSockets (Connection, receiveData, sendTextData)
import Network.WebSockets.Connection (pingThread)
import Olymp.Effect.Log (Log, logError)
import Olymp.Schema (SessionId, User)
import Olymp.Tournament.Base

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

type TourneyEffs
  = '[ AtomicState (Tournament NodeId)
     , AtomicState (Map Int Connection)
     , Bracket
     , Log
     , Embed IO
     ]

initialTournament :: Tournament NodeId
initialTournament = (createTournament ps) { userFocus = Just 2 }
  where
    ps = M.fromList
      [ (0 :: PlayerId, Player "Dan a Barča" "Daniel a Barbora Borůvkovi")
      , (1, Player "Tomáš a Monika" "Tomáš Opichal a Monika Maňásková")
      , (2, Player "Roman a Anička" "Roman Pecha a Anna Banková")
      , (3, Player "Vilda a Hanka" "Vilém Šír a Hana-Anna Šišková")
      ]

withSocketLoop :: Effs TourneyEffs m => FromJSON a => Connection -> (a -> m ()) -> m ()
withSocketLoop c f = do
  welcomeMsg <- encode . StateMsg <$> atomicGet
  embed $ do
    pingThread c 10 (pure ())
    sendTextData c welcomeMsg
  k <- atomicState' @(Map Int Connection) (\m -> let k = 1 + foldr max 0 (M.keys m) in (M.insert k c m, k))
  flip finally (atomicModify' @(Map Int Connection) $ M.delete k) $ forever $
    embed (receiveData c) >>= either (logError . T.pack) f . eitherDecode'

updateNode ::
     Effs TourneyEffs m
  => NodeId
  -> (TournamentNodeF NodeId NodeId -> Maybe (TournamentNodeF NodeId NodeId))
  -> Text
  -> m ()
updateNode nid f msg = do
  mErr <- atomicState' @(Tournament NodeId) $ \t ->
    case M.lookup nid (nodes t) of
      Nothing -> (t, Just $ "Unknown node in " <> msg)
      Just n -> case f n of
        Nothing -> (t, Just $ "Node not ready for " <> msg)
        Just new -> (t & field @"nodes" . ix nid .~ new, Nothing)
  maybe broadcastState logError mErr

tournamentSocket :: Effs TourneyEffs m => Connection -> m ()
tournamentSocket c = withSocketLoop c $ \msg -> case msg of
  Vote nid left right ->
    updateNode nid (setScore (+ left) (+ right)) (tshow msg)

tournamentAdminSocket :: Effs TourneyEffs m => (SessionId, Entity User) -> Connection -> m ()
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
    embed $ mapM_ (`sendTextData` encode ResetAll) conns
    broadcastState
  CloseVoting nid -> do
    mErr <- atomicState' @(Tournament NodeId) $ \t ->
      case M.lookup nid (nodes t) of
        Nothing -> (t, Just $ "Unknown node in " <> tshow msg)
        Just n -> case closeVoting n of
          Nothing -> (t, Just $ "Node not ready for " <> tshow msg)
          Just closed ->
            let nodes' = nodes t & ix nid .~ closed
                loser = fromRight 0 $ fromMaybe 0 . getLoser <$> treeify nodes' (nodeId closed)
                lb = maybe M.empty
                      (withTree (propagateWinners . snd . fillPlayers [loser]) nodes')
                      (losersRoot t)
                wb = withTree propagateWinners nodes' (winnersRoot t)
            in (t & field @"nodes" .~ wb <> lb, Nothing)
    maybe broadcastState logError mErr

broadcastState :: Effs TourneyEffs m => m ()
broadcastState = do
  msg <- encode . StateMsg <$> atomicGet @(Tournament NodeId)
  conns <- atomicGet @(Map Int Connection)
  embed $ mapM_ (`sendTextData` msg) conns

tshow :: Show a => a -> Text
tshow = T.pack . show
