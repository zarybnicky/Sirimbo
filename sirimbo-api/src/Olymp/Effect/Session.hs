{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Effect.Session
  ( SessionEff(..)
  , getSessionById
  , deleteSession
  , runSessionEffPersistent
  ) where

import Control.Effect (Eff, SimpleInterpreterFor, interpretSimple, send)
import Database.Persist (delete, get)
import Olymp.Effect.Database (WithDbFor, query)
import Olymp.Schema (Session, SessionId)

data SessionEff m a where
    GetSessionById :: SessionId -> SessionEff m (Maybe Session)
    DeleteSession :: SessionId -> SessionEff m ()

getSessionById :: Eff SessionEff m => SessionId -> m (Maybe Session)
getSessionById = send . GetSessionById

deleteSession :: Eff SessionEff m => SessionId -> m ()
deleteSession = send . DeleteSession

runSessionEffPersistent :: forall db m. WithDbFor Session db m => SimpleInterpreterFor SessionEff m
runSessionEffPersistent = interpretSimple $ \case
  GetSessionById sid -> query @db (get sid)
  DeleteSession sid -> query @db (delete sid)
