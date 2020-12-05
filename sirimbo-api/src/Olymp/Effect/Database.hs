{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Effect.Database
  ( Database(..)
  , WithDb
  , WithDbFor
  , query
  , runDatabasePool
  ) where

import Control.Effect
  (Eff, Effs, Embed, SimpleInterpreterFor, send, embed, interpretSimple)
import Control.Effect.Bracket (Bracket, bracket)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Pool (Pool, takeResource, putResource)
import Database.Persist.Class (PersistStoreRead)
import Database.Persist.Sql (SqlBackendCanWrite, PersistEntityBackend)

data Database backend m a where
  Query :: ReaderT backend IO a -> Database backend m a

query :: Eff (Database backend) m => ReaderT backend IO a -> m a
query = send . Query

runDatabasePool
  :: Effs '[Bracket, Embed IO] m => Pool backend -> SimpleInterpreterFor (Database backend) m
runDatabasePool pool = interpretSimple $ \case
  Query f -> bracket
    (embed $ takeResource pool)
    (\(r, lp) -> embed $ putResource lp r)
    (\(r, _) -> embed $ runReaderT f r)

type WithDb db r = (PersistStoreRead db, SqlBackendCanWrite db, Eff (Database db) r)
type WithDbFor e db r = (WithDb db r, PersistEntityBackend e ~ db)
