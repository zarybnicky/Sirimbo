{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Olymp.Tournament.Base
  ( Tournament(..)
  , TournamentNodeF(..)
  , TournamentResult(..)
  , DuelResult(..)
  , Player(..)
  , PlayerId(..)
  , NodeId(..)
  , createTournament
  , fillPlayers
  , fillWaiting
  , listify
  , treeify
  , mapId
  , openVoting
  , closeVoting
  , setScore
  , propagateWinners
  , getWinner
  , getLoser
  , nodeId
  , withTree
  , withTournament
  , Fix
  , unFix
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, runState, evalState, gets, put)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Fix (Fix(..))
import Data.Foldable (for_)
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Functor.Classes.Generic (liftEqDefault, liftShowsPrecDefault)
import Data.Functor.Foldable
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic, Generic1)

-- data Ruleset
--   = SingleElimination
--   | DoubleElimination
--   deriving (Show, Eq, Ord)

-- data Bracket
--   = WinnersBracket
--   | LosersBracket
--   deriving (Show, Eq, Ord)

data TournamentResult p = TournamentResult
  { player :: p
  , placement :: Int
  , wins :: Int
  , total :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

data Tournament id = Tournament
  { nodes :: Map id (TournamentNodeF id id)
  , winnersRoot :: id
  , losersRoot :: Maybe id
  , dashboardFocus :: Maybe id
  , userFocus :: Maybe id
  , tournamentPlayers :: Map PlayerId Player
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype PlayerId = PlayerId Int
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum, Num, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data Player = Player
  { shortName :: String
  , longName :: String
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype NodeId = NodeId Int
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum, Num, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data DuelResult = DuelResult
  { leftPlayer :: PlayerId
  , rightPlayer :: PlayerId
  , leftScore :: Int
  , rightScore :: Int
  , victor :: Maybe PlayerId
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data TournamentNodeF id f
  = SeedNode id PlayerId
  | SeedWaitingNode id
  | DuelWaitingNode id (Maybe PlayerId) (Maybe PlayerId) f f
  | DuelFinishedNode id DuelResult f f
  deriving stock (Eq, Show, Generic, Generic1, Functor, Foldable, Traversable)
  deriving anyclass (FromJSON, ToJSON)

instance Eq a => Eq1 (TournamentNodeF a) where
  liftEq = liftEqDefault
instance Show a => Show1 (TournamentNodeF a) where
  liftShowsPrec = liftShowsPrecDefault

openVoting :: TournamentNodeF id f -> Maybe (TournamentNodeF id f)
openVoting = \case
  DuelWaitingNode n (Just left) (Just right) l r ->
    Just $ DuelFinishedNode n (DuelResult left right 0 0 Nothing) l r
  DuelFinishedNode n res l r ->
    let newRes = res { victor = Nothing }
    in Just $ DuelFinishedNode n newRes l r
  _ -> Nothing

closeVoting :: TournamentNodeF id f -> Maybe (TournamentNodeF id f)
closeVoting = \case
  DuelFinishedNode n res@DuelResult{victor=Nothing} l r ->
    let victor = case leftScore res `compare` rightScore res of
          GT -> Just (leftPlayer res)
          EQ -> Nothing
          LT -> Just (rightPlayer res)
    in (\v -> DuelFinishedNode n res { victor = Just v } l r) <$> victor
  _ -> Nothing

setScore :: (Int -> Int) -> (Int -> Int) -> TournamentNodeF id f -> Maybe (TournamentNodeF id f)
setScore left right = \case
  DuelFinishedNode n res l r ->
    let newRes = res
          { leftScore = left (leftScore res)
          , rightScore = right (rightScore res)
          }
    in Just $ DuelFinishedNode n newRes l r
  _ -> Nothing

mapId ::
     Applicative m
  => (TournamentNodeF id f -> id -> m id')
  -> TournamentNodeF id f
  -> m (TournamentNodeF id' f)
mapId f node = case node of
  SeedNode n p -> (`SeedNode` p) <$> f node n
  SeedWaitingNode n -> SeedWaitingNode <$> f node n
  DuelWaitingNode n lp rp l r -> (\n' -> DuelWaitingNode n' lp rp l r) <$> f node n
  DuelFinishedNode n res l r -> (\n' -> DuelFinishedNode n' res l r) <$> f node n

nodeId :: TournamentNodeF id f -> id
nodeId = \case
  SeedNode n _ -> n
  SeedWaitingNode n -> n
  DuelWaitingNode n _ _ _ _ -> n
  DuelFinishedNode n _ _ _ -> n

withTree ::
     Ord id
  => (Fix (TournamentNodeF id) -> Fix (TournamentNodeF id))
  -> Map id (TournamentNodeF id id)
  -> id
  -> Map id (TournamentNodeF id id)
withTree f m = either (const M.empty) (listify . f) . treeify m

withTournament ::
     Ord id
  => (Fix (TournamentNodeF id) -> Fix (TournamentNodeF id))
  -> Tournament id
  -> Tournament id
withTournament f t@Tournament {..} =
  let wb = withTree f nodes winnersRoot
      lb = maybe M.empty (withTree f nodes) losersRoot
  in t { nodes = wb <> lb }

listify :: Ord id => Fix (TournamentNodeF id) -> Map id (TournamentNodeF id id)
listify = para $ \case
  SeedNode n p -> M.singleton n (SeedNode n p)
  SeedWaitingNode n -> M.singleton n (SeedWaitingNode n)
  DuelWaitingNode n lp rp (Fix l, ln) (Fix r, rn) ->
    ln <> rn <> M.singleton n (DuelWaitingNode n lp rp (nodeId l) (nodeId r))
  DuelFinishedNode n res (Fix l, ln) (Fix r, rn) ->
    ln <> rn <> M.singleton n (DuelFinishedNode n res (nodeId l) (nodeId r))

treeify ::
     Ord id
  => Map id (TournamentNodeF id id)
  -> id
  -> Either id (Fix (TournamentNodeF id))
treeify m root =
  fmap embed . traverse (treeify m) =<< maybe (Left root) Right (M.lookup root m)

createTournament :: Map PlayerId Player -> Tournament NodeId
createTournament playerMap = flip evalState (infiniteStream $ 1:|[2..]) $ do
  let keys = M.keys playerMap
  let n = length keys
  wb <- flip evalState keys . fillSeeds <$> makeBracket n
  lb <- if n > 2 then Just <$> makeBracket (n - 2) else pure Nothing
  let nodes' = listify wb <> maybe M.empty listify lb
  pure $ Tournament nodes' (nodeId $ unFix wb) (nodeId . unFix <$> lb) Nothing Nothing playerMap

makeBracket :: Int -> State (Stream id) (Fix (TournamentNodeF id))
makeBracket n
  | n == 1 = Fix . SeedWaitingNode <$> headStream
  | otherwise = do
      self <- headStream
      l <- makeBracket (n `div` 2 + n `mod` 2)
      r <- makeBracket (n `div` 2)
      pure (Fix $ DuelWaitingNode self Nothing Nothing l r)

fillPlayers ::
     [PlayerId]
  -> Fix (TournamentNodeF id)
  -> ([PlayerId], Fix (TournamentNodeF id))
fillPlayers keys = (\(a, b) -> (b, a)) . flip runState keys . fillSeeds

fillSeeds ::
     Fix (TournamentNodeF id)
  -> State [PlayerId] (Fix (TournamentNodeF id))
fillSeeds (Fix node) = Fix <$> case node of
  SeedWaitingNode nid -> headState >>= \case
    Nothing -> pure node
    Just p -> pure (SeedNode nid p)
  DuelWaitingNode nid lp rp l r -> DuelWaitingNode nid lp rp <$> fillSeeds l <*> fillSeeds r
  _ -> pure node

fillWaiting :: PlayerId -> Fix (TournamentNodeF id) -> Maybe (Fix (TournamentNodeF id))
fillWaiting p (Fix node) = Fix <$> case node of
  DuelWaitingNode n Nothing rp l r -> Just $ DuelWaitingNode n (Just p) rp l r
  DuelWaitingNode n lp Nothing l r -> Just $ DuelWaitingNode n lp (Just p) l r
  DuelWaitingNode n lp rp l r ->
    case fillWaiting p l of
      Just new -> Just $ DuelWaitingNode n lp rp new r
      Nothing -> case fillWaiting p r of
        Just new -> Just $ DuelWaitingNode n lp rp l new
        Nothing -> Nothing
  _ -> Nothing

propagateWinners :: Fix (TournamentNodeF id) -> Fix (TournamentNodeF id)
propagateWinners (Fix node) = Fix $ case node of
  DuelWaitingNode n lp rp l r ->
    let
      lc = propagateWinners l
      rc = propagateWinners r
      left = lp <|> getWinner lc
      right = rp <|> getWinner rc
    in DuelWaitingNode n left right lc rc
  _ -> node

getWinner :: Fix (TournamentNodeF id) -> Maybe PlayerId
getWinner (Fix node) = case node of
  SeedNode _ p -> Just p
  SeedWaitingNode _ -> Nothing
  DuelWaitingNode{} -> Nothing
  DuelFinishedNode _ res _ _ -> victor res

getLoser :: Fix (TournamentNodeF id) -> Maybe PlayerId
getLoser (Fix node) = case node of
  SeedNode _ p -> Just p
  SeedWaitingNode _ -> Nothing
  DuelWaitingNode{} -> Nothing
  DuelFinishedNode _ DuelResult{..} _ _ ->
    case leftScore `compare` rightScore of
      GT -> Just rightPlayer
      EQ -> Nothing
      LT -> Just leftPlayer

-- upcoming battles
-- in-progress battles (victor == Nothing)
-- get battle by ID (ID == path to node? 1=left, 2=right)
-- update score


headState :: State [a] (Maybe a)
headState = do
  s <- gets uncons
  for_ s (put . snd)
  pure $ fmap fst s

headStream :: State (Stream a) a
headStream = do
  (s, ss) <- gets takeStream
  put ss
  pure s


data Stream a = MkStream (NonEmpty a) (NonEmpty a)

infiniteStream :: NonEmpty a -> Stream a
infiniteStream as = MkStream as as

takeStream :: Stream a -> (a, Stream a)
takeStream (MkStream (a:|as) b) = case NE.nonEmpty as of
  Nothing -> (a, MkStream b b)
  Just as' -> (a, MkStream as' b)
