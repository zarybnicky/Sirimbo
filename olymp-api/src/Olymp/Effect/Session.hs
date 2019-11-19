{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Effect.Session
  ( SessionEff(..)
  , getSessionById
  , deleteSession
  , runSessionEffPersistent
  ) where

import Data.Kind (Type)
import Database.Persist (delete, get)
import Olymp.Effect.Database (WithDbFor, query)
import Olymp.Schema (Session, SessionId)
import Polysemy (Sem, interpret, makeSem)

data SessionEff (m :: Type -> Type) (a :: Type) where
    GetSessionById :: SessionId -> SessionEff m (Maybe Session)
    DeleteSession :: SessionId -> SessionEff m ()

makeSem ''SessionEff

runSessionEffPersistent :: WithDbFor Session db r => Sem (SessionEff ': r) a -> Sem r a
runSessionEffPersistent = interpret $ \case
  GetSessionById sid -> query (get sid)
  DeleteSession sid -> query (delete sid)
