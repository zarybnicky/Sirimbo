{-# LANGUAGE OverloadedStrings #-}

module Olymp.Prelude
  ( module X
  , module Olymp.Prelude
  ) where

import Control.Effect as X (Effs)
import Control.Effect.Error as X (Error, throw)
import Data.Aeson as X (ToJSON)
import Data.Bifunctor as X (bimap)
import Data.OpenApi as X (ToSchema)
import Data.Text as X (Text)
import Database.Persist.Sql as X (Entity (..), get, updateGet, (=.))
import GHC.Generics as X (Generic)
import Olymp.Auth as X (PhpAuth)
import Olymp.Effect.Database as X (Database, query)
import Olymp.Schema as X (EntityField (..), SessionId, User)
import Servant as X ((:>), (:<|>)(..), Capture, Get, Post, JSON, ServerT, ServerError, err404)

textToBool :: Text -> Bool
textToBool "0" = False
textToBool _ = True

boolToText :: Bool -> Text
boolToText True = "1"
boolToText False = "0"
