{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Olymp.Tournament
  ( Ruleset(..)
  , Bracket(..)
  , Tournament(..)
  , TournamentNode
  , TournamentNodeF(..)
  , TournamentResult(..)
  , DuelResult(..)
  , createTournament
  , fillPlayers
  , listify
  , treeify
  ) where

import Data.Foldable (for_)
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.Functor.Classes.Generic (liftEqDefault, liftShowsPrecDefault)
import Data.Functor.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic, Generic1)
import Polysemy (Member, Sem, run, reinterpret)
import Polysemy.Input
import Polysemy.State

data Ruleset
  = SingleElimination
  -- DoubleElimination
  deriving (Show, Eq, Ord)

data Bracket
  = WinnersBracket
  | LosersBracket
  deriving (Show, Eq, Ord)

data TournamentResult p = TournamentResult
  { player :: p
  , placement :: Int
  , wins :: Int
  , total :: Int
  } deriving (Show, Generic)

data Tournament id = Tournament
  { winnersBracket :: TournamentNode id
  , losersBracket :: Maybe (TournamentNode id)
  } deriving (Eq, Show, Generic)

newtype PlayerId = PlayerId String
  deriving (Eq, Show, Generic)
newtype Player = Player String
  deriving (Eq, Show, Generic)
newtype FreshNodeId = FreshNodeId Int
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Enum, Num)
newtype NodeId = NodeId String
  deriving (Eq, Show, Generic)

data DuelResult = DuelResult
  { score :: Map Player Int
  , victor :: Maybe Player
  } deriving (Eq, Show, Generic)

type TournamentNode id = Fix (TournamentNodeF id)
data TournamentNodeF id f
  = SeedNode id Player
  | SeedWaitingNode id
  | DuelWaitingNode id f f
  | DuelReadyNode id [Player] f f
  | DuelFinishedNode id DuelResult f f
  deriving stock (Eq, Show, Generic, Generic1, Functor, Foldable, Traversable)

instance Eq a => Eq1 (TournamentNodeF a) where
  liftEq = liftEqDefault
instance Show a => Show1 (TournamentNodeF a) where
  liftShowsPrec = liftShowsPrecDefault

nodeId :: TournamentNodeF id f -> id
nodeId = \case
  SeedNode n _ -> n
  SeedWaitingNode n -> n
  DuelWaitingNode n _ _ -> n
  DuelReadyNode n _ _ _ -> n
  DuelFinishedNode n _ _ _ -> n

listify :: Ord id => TournamentNode id -> Map id (TournamentNodeF id id)
listify = para $ \case
  SeedNode n p -> M.singleton n (SeedNode n p)
  SeedWaitingNode n -> M.singleton n (SeedWaitingNode n)
  DuelWaitingNode n (Fix l, ln) (Fix r, rn) -> ln <> rn <> M.singleton n (DuelWaitingNode n (nodeId l) (nodeId r))
  DuelReadyNode n ps (Fix l, ln) (Fix r, rn) -> ln <> rn <> M.singleton n (DuelReadyNode n ps (nodeId l) (nodeId r))
  DuelFinishedNode n res (Fix l, ln) (Fix r, rn) -> ln <> rn <> M.singleton n (DuelFinishedNode n res (nodeId l) (nodeId r))

treeify :: (Ord id) => Map id (TournamentNodeF id id) -> id -> Either id (TournamentNode id)
treeify m root =
  fmap embed . traverse (treeify m) =<< maybe (Left root) Right (M.lookup root m)

-- cataM :: (Recursive t, Traversable (Base t), Monad m) => (Base t a -> m a) -> (t -> m a)
-- cataM phi = fix (fmap (phi <=<) (\x y -> mapM x (project y)))

createTournament :: Ruleset -> [Player] -> Tournament FreshNodeId
createTournament SingleElimination players = run . runListInputForever (1:|[2..]) $ do
  let n = length players
  wb <- runInputList players (fillSeeds =<< makeBracket n)
  lb <- if n > 2 then Just <$> makeBracket (n - 2) else pure Nothing
  pure $ Tournament wb lb

makeBracket :: Member (Input id) r => Int -> Sem r (TournamentNode id)
makeBracket n
  | n == 1 = Fix . SeedWaitingNode <$> input
  | otherwise = do
      self <- input
      l <- makeBracket (n `div` 2 + n `mod` 2)
      r <- makeBracket (n `div` 2)
      pure (Fix $ DuelWaitingNode self l r)

fillPlayers :: [Player] -> TournamentNode id -> ([Player], TournamentNode id)
fillPlayers players = run . runInputList' players . fillSeeds

fillSeeds ::
     Member (Input (Maybe Player)) r
  => TournamentNode id
  -> Sem r (TournamentNode id)
fillSeeds (Fix node) = Fix <$> case node of
  SeedWaitingNode nid -> input >>= \case
    Nothing -> pure node
    Just p -> pure (SeedNode nid p)
  DuelWaitingNode nid l r -> DuelWaitingNode nid <$> fillSeeds l <*> fillSeeds r
  _ -> pure node

-- upcoming battles
-- in-progress battles (victor == Nothing)
-- get battle by ID (ID == path to node? 1=left, 2=right)
-- update score


data Stream a = MkStream (NonEmpty a) (NonEmpty a)

infiniteStream :: NonEmpty a -> Stream a
infiniteStream as = MkStream as as

takeStream :: Stream a -> (a, Stream a)
takeStream (MkStream (a:|as) b) = case NE.nonEmpty as of
  Nothing -> (a, MkStream b b)
  Just as' -> (a, MkStream as' b)

runListInputForever :: NonEmpty i -> Sem (Input i ': r) a -> Sem r a
runListInputForever is = fmap snd . runState (infiniteStream is) . reinterpret (\case
  Input -> do
    (s, ss) <- gets takeStream
    put ss
    pure s)

runInputList' :: [i] -> Sem (Input (Maybe i) ': r) a -> Sem r ([i], a)
runInputList' is = runState is . reinterpret (\case
  Input -> do
    s <- gets uncons
    for_ s (put . snd)
    pure $ fmap fst s)
