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

module Olymp.Effect.User
  ( UserEff(..)
  , getUserById
  , runUserEffPersistent
  ) where

import Data.Kind (Type)
import Database.Persist (Key, get)
import Olymp.Effect.Database (WithDbFor, query)
import Olymp.Schema (User)
import Polysemy (Sem, interpret, makeSem)

data UserEff (m :: Type -> Type) (a :: Type) where
    GetUserById :: Key User -> UserEff m (Maybe User)

$(makeSem ''UserEff)

runUserEffPersistent :: WithDbFor User db r => Sem (UserEff ': r) a -> Sem r a
runUserEffPersistent = interpret $ \case
  GetUserById uid -> query (get uid)
