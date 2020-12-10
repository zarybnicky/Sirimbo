{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}

module Olymp.Effect.Session
  ( SessionEff(..)
  , getSessionById
  , deleteSession
  , runSessionEffPersistent
  ) where

import Control.Effect (Eff, SimpleInterpreterFor, interpretSimple, send)
import Database.Persist (delete, get)
import Olymp.Effect.Database (Database, query)
import Olymp.Schema (Session, SessionId)

data SessionEff m a where
    GetSessionById :: SessionId -> SessionEff m (Maybe Session)
    DeleteSession :: SessionId -> SessionEff m ()

getSessionById :: Eff SessionEff m => SessionId -> m (Maybe Session)
getSessionById = send . GetSessionById

deleteSession :: Eff SessionEff m => SessionId -> m ()
deleteSession = send . DeleteSession

runSessionEffPersistent :: Eff Database m => SimpleInterpreterFor SessionEff m
runSessionEffPersistent = interpretSimple $ \case
  GetSessionById sid -> query (get sid)
  DeleteSession sid -> query (delete sid)
