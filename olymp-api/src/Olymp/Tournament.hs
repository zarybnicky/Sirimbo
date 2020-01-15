{-# LANGUAGE DeriveGeneric #-}

module Olymp.Tournament
  ( Ruleset(..)
  , Bracket(..)
  , Tournament(..)
  , TournamentNode(..)
  , TournamentResult(..)
  , DuelResult(..)
  , createTournament
  , createTournamentList
  , fillPlayers
  ) where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)

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

data Tournament p r = Tournament
  { winnersBracket :: TournamentNode p r
  , losersBracket :: Maybe (TournamentNode p r)
  } deriving (Show, Generic)

data TournamentNode p r
  = SeedNode p
  | SeedWaitingNode
  | DuelWaitingNode [TournamentNode p r]
  | DuelReadyNode [p] [TournamentNode p r]
  | DuelFinishedNode [p] r [TournamentNode p r]
  deriving (Show, Generic)

newtype DuelResult p = DuelVictor p
  deriving (Show, Generic)

createTournamentList :: Ruleset -> [p] -> Maybe (Tournament p (DuelResult p))
createTournamentList r ps = createTournament r <$> nonEmpty ps

createTournament :: Ruleset -> NonEmpty p -> Tournament p (DuelResult p)
createTournament SingleElimination players = Tournament wb lb
  where
    n = length players
    wb = fillPlayers players (makeBracket n)
    lb = if n > 2 then Just $ makeBracket (n - 2) else Nothing

makeBracket :: Int -> TournamentNode p r
makeBracket n
  | n == 1 = SeedWaitingNode
  | otherwise = DuelWaitingNode [makeBracket (n `div` 2 + n `mod` 2), makeBracket (n `div` 2)]

fillPlayers :: NonEmpty p -> TournamentNode p r -> TournamentNode p r
fillPlayers ps = either id snd . fillSeeds ps

fillSeeds :: NonEmpty p -> TournamentNode p r -> Either (TournamentNode p r) (NonEmpty p, TournamentNode p r)
fillSeeds ps node = case node of
  SeedWaitingNode -> case nonEmpty (NE.tail ps) of
    Nothing -> Left (SeedNode (NE.head ps))
    Just ps' -> Right (ps', SeedNode (NE.head ps))
  DuelWaitingNode cs -> foldChildren ps cs
  SeedNode _ -> Right (ps, node)
  DuelReadyNode _ _ -> Right (ps, node)
  DuelFinishedNode{} -> Right (ps, node)
  where
    foldChildren ps' cs =
      let (ps'', cs') = foldl' step (Just ps', []) cs
      in case ps'' of
        Nothing -> Left (DuelWaitingNode cs')
        Just ps''' -> Right (ps''', DuelWaitingNode cs')
    step (ps', acc) c = case ps' of
      Nothing -> (Nothing, acc ++ [c])
      Just ps'' -> case fillSeeds ps'' c of
        Left c' -> (Nothing, acc ++ [c'])
        Right (ps''', c') -> (Just ps''', acc ++ [c'])
