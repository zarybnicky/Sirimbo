{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Effect.Error
  ( AppError(..)
  , AuthError(..)
  , throwAuth
  , runAppErrorToError
  ) where

import Control.Effect
import Control.Effect.Error (Error, throw)
import Data.Kind (Type)
import Servant.Server.Internal.ServerError (ServerError, err401, err403)

data AuthError
  = ErrNotLoggedIn
  | ErrMissingPermission

data AppError (m :: Type -> Type) (a :: Type) where
    ThrowAuth :: AuthError -> AppError m a

throwAuth :: Eff AppError m => AuthError -> m a
throwAuth = send . ThrowAuth

runAppErrorToError :: Eff (Error ServerError) m => SimpleInterpreterFor AppError m
runAppErrorToError = interpretSimple $ \case
  ThrowAuth e -> case e of
    ErrNotLoggedIn -> throw err401
    ErrMissingPermission -> throw err403
