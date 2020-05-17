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

module Olymp.Effect.Error
  ( AppError(..)
  , AuthError(..)
  , throwAuth
  , runAppErrorToError
  ) where

import Data.Kind (Type)
import Polysemy (Member, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Servant.Server.Internal.ServerError (ServerError, err401, err403)

data AuthError
  = ErrNotLoggedIn
  | ErrMissingPermission

data AppError (m :: Type -> Type) (a :: Type) where
    ThrowAuth :: AuthError -> AppError m a

$(makeSem ''AppError)

runAppErrorToError :: Member (Error ServerError) r => Sem (AppError ': r) a -> Sem r a
runAppErrorToError = interpret $ \case
  ThrowAuth e -> case e of
    ErrNotLoggedIn -> throw err401
    ErrMissingPermission -> throw err403
