{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}

module Olymp.Effect.User
  ( UserEff(..)
  , getUserById
  , runUserEffPersistent
  ) where

import Control.Effect (Eff, SimpleInterpreterFor, interpretSimple, send)
import Database.Persist (Entity, Key, getEntity)
import Olymp.Effect.Database (Database, query)
import Olymp.Schema (User)

data UserEff m a where
    GetUserById :: Key User -> UserEff m (Maybe (Entity User))

getUserById :: Eff UserEff m => Key User -> m (Maybe (Entity User))
getUserById = send . GetUserById

runUserEffPersistent :: Eff Database m => SimpleInterpreterFor UserEff m
runUserEffPersistent = interpretSimple $ \case
  GetUserById uid -> query (getEntity uid)
