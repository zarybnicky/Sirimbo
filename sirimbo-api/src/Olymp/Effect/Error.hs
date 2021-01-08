{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Olymp.Effect.Error
  ( AppError(..)
  , AuthError(..)
  , throwAuth
  , throw404
  , runAppErrorToError
  ) where

import Control.Effect
import Control.Effect.Error (Error, throw)
import qualified Data.ByteString.Lazy as BS
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Servant.Server.Internal.ServerError
  (ServerError(..), err401, err403, err404)

data AuthError
  = ErrNotLoggedIn
  | ErrMissingPermission

data AppError (m :: Type -> Type) (a :: Type) where
  ThrowAuth :: AuthError -> AppError m a
  Throw404 :: Text -> AppError m a

throwAuth :: Eff AppError m => AuthError -> m a
throwAuth = send . ThrowAuth

throw404 :: Eff AppError m => Text -> m a
throw404 = send . Throw404

runAppErrorToError :: Eff (Error ServerError) m => SimpleInterpreterFor AppError m
runAppErrorToError = interpretSimple $ \case
  ThrowAuth e -> case e of
    ErrNotLoggedIn -> throw err401
    ErrMissingPermission -> throw err403
  Throw404 t -> throw err404 { errBody = BS.fromStrict $ T.encodeUtf8 t }
