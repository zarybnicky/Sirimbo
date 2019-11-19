{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Effect.Database
  ( Database(..)
  , WithDb
  , WithDbFor
  , query
  , runDatabasePool
  ) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Pool (Pool, takeResource, putResource)
import Database.Persist.Sql (SqlBackendCanWrite, PersistEntityBackend)
import Polysemy (Embed, Member, Members, Sem, embed, interpret, makeSem)
import Polysemy.Resource (Resource, bracket)

data Database backend m a where
    Query :: ReaderT backend IO a -> Database backend m a

$(makeSem ''Database)

type WithDb db r = (SqlBackendCanWrite db, Member (Database db) r)
type WithDbFor e db r = (WithDb db r, PersistEntityBackend e ~ db)

runDatabasePool ::
     Members '[Embed IO, Resource] r
  => Pool backend
  -> Sem (Database backend ': r) a
  -> Sem r a
runDatabasePool pool = interpret $ \case
  Query f -> bracket
    (embed $ takeResource pool)
    (\(r, lp) -> embed $ putResource lp r)
    (\(r, _) -> embed $ runReaderT f r)
