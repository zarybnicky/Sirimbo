{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Prelude
  ( module X,
    module Olymp.Prelude,
  )
where

import Control.Effect as X (Effs)
import Control.Effect.Error as X (Error, throw)
import Control.Lens ((&), (?~))
import Data.Aeson as X (ToJSON)
import Data.Bifunctor as X (bimap)
import Data.OpenApi as X (ToSchema, allOperations, operationId, operationsOf)
import Data.Proxy as X (Proxy (..))
import Data.Text as X (Text)
import qualified Data.Text as T
import Database.Persist.Sql as X (Entity (..), get, selectList, updateGet, (=.))
import GHC.Generics as X (Generic)
import GHC.TypeLits as X (KnownSymbol, Symbol, symbolVal)
import Olymp.Auth as X (PhpAuth)
import Olymp.Effect.Database as X (Database, query)
import Olymp.Schema as X (EntityField (..), SessionId, User)
import Servant as X (Capture, Get, HasServer (..), JSON, Post, QueryParam, ServerError, ServerT, err404, (:<|>) (..), (:>))
import Servant.OpenApi (HasOpenApi (..))

textToBool :: Text -> Bool
textToBool "0" = False
textToBool _ = True

boolToText :: Bool -> Text
boolToText True = "1"
boolToText False = "0"

data OpId (a :: Symbol)

instance (HasOpenApi a, KnownSymbol s) => HasOpenApi (OpId s :> a) where
  toOpenApi _ = toOpenApi @a Proxy & allOperations . operationId ?~ T.pack (symbolVal $ Proxy @s)

instance HasServer api ctx => HasServer (OpId s :> api) ctx where
  type ServerT (OpId s :> api) m = ServerT api m
  route Proxy = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)
