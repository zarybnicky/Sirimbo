{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.Map.Strict (Map)
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

data Tournament id p = Tournament
  { winnersBracket :: TournamentNode id p []
  , losersBracket :: Maybe (TournamentNode id p [])
  } deriving (Show, Generic)

data TournamentNode id p (f :: * -> *)
  = SeedNode id p
  | SeedWaitingNode id
  | DuelWaitingNode id (f (TournamentNode id p f))
  | DuelReadyNode id [p] (f (TournamentNode id p f))
  | DuelFinishedNode id [p] (DuelResult p) (f (TournamentNode id p f))

deriving instance (Show id, Show p, Show (f (TournamentNode id p f))) => Show (TournamentNode id p f)
deriving instance Generic (TournamentNode id p f)

data DuelResult p = DuelResult
  { score :: Map p Int
  , victor :: Maybe p
  } deriving (Show, Generic)

createTournamentList :: Ruleset -> [p] -> Maybe (Tournament id p)
createTournamentList r ps = createTournament r <$> nonEmpty ps

createTournament :: Ruleset -> NonEmpty p -> Tournament id p
createTournament SingleElimination players = Tournament wb lb
  where
    n = length players
    wb = fillPlayers players (makeBracket n)
    lb = if n > 2 then Just $ makeBracket (n - 2) else Nothing

makeBracket :: Int -> TournamentNode id p []
makeBracket n
  | n == 1 = SeedWaitingNode
  | otherwise = DuelWaitingNode [makeBracket (n `div` 2 + n `mod` 2), makeBracket (n `div` 2)]

fillPlayers :: NonEmpty p -> TournamentNode id p [] -> TournamentNode id p []
fillPlayers ps = either id snd . fillSeeds ps

fillSeeds :: NonEmpty p -> TournamentNode id p [] -> Either (TournamentNode id p []) (NonEmpty p, TournamentNode id p [])
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

-- upcoming battles
-- in-progress battles (victor == Nothing)
-- get battle by ID (ID == path to node? 1=left, 2=right)
-- update score
