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

module Olymp.Effect.User
  ( UserEff(..)
  , getUserById
  , runUserEffPersistent
  ) where

import Control.Effect (Eff, SimpleInterpreterFor, interpretSimple, send)
import Database.Persist (Key, get)
import Olymp.Effect.Database (Database, query)
import Olymp.Schema (User)

data UserEff m a where
    GetUserById :: Key User -> UserEff m (Maybe User)

getUserById :: Eff UserEff m => Key User -> m (Maybe User)
getUserById = send . GetUserById

runUserEffPersistent :: Eff Database m => SimpleInterpreterFor UserEff m
runUserEffPersistent = interpretSimple $ \case
  GetUserById uid -> query (get uid)
