{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Olymp.Effect.Database
  ( Database(..)
  , query
  , runDatabasePool
  ) where

import Control.Effect (Eff, Embed, SimpleInterpreterFor, embed, interpretSimple, liftBase, send)
import Control.Effect.Bracket (bracket, bracketToIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Pool (Pool, takeResource, putResource)
import Database.Persist.Sql (SqlBackend)
import Control.Effect.Mask (MonadMask)

data Database m a where
  Query :: ReaderT SqlBackend IO a -> Database m a

query :: Eff Database m => ReaderT SqlBackend IO a -> m a
query = send . Query

runDatabasePool :: (Eff (Embed IO) m, MonadMask m) => Pool SqlBackend -> SimpleInterpreterFor Database m
runDatabasePool pool = interpretSimple $ \case
  Query f -> liftBase $ bracketToIO $ bracket
    (embed $ takeResource pool)
    (\(r, lp) -> embed $ putResource lp r)
    (\(r, _) -> embed $ runReaderT f r)
