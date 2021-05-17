{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module Olymp.Schema.Orphans () where

import Data.Aeson (eitherDecodeStrict, encode, Value)
import Data.Text (Text)
import Database.Persist (PersistField(..))
import Database.Persist.Sql (SqlType(SqlOther), PersistValue(..), PersistFieldSql(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

instance PersistField Value where
  toPersistValue = PersistLiteral . BSL.toStrict . encode
  fromPersistValue (PersistText t) =
    case eitherDecodeStrict $ TE.encodeUtf8 t of
      Left str -> Left $ fromPersistValueParseError "FromJSON" t $ T.pack str
      Right v -> Right v
  fromPersistValue (PersistByteString bs) =
    case eitherDecodeStrict bs of
      Left str -> Left $ fromPersistValueParseError "FromJSON" bs $ T.pack str
      Right v -> Right v
  fromPersistValue x = Left $ fromPersistValueError "FromJSON" "string or bytea" x

instance PersistFieldSql Value where
  sqlType _ = SqlOther "BLOB"

fromPersistValueError :: Text -> Text -> PersistValue -> Text
fromPersistValueError haskellType databaseType received =
  T.concat
    [ "Failed to parse Haskell type `",
      haskellType,
      "`; expected ",
      databaseType,
      " from database, but received: ",
      T.pack (show received),
      ". Potential solution: Check that your database schema matches your Persistent model definitions."
    ]

fromPersistValueParseError :: Show a => Text -> a -> Text -> Text
fromPersistValueParseError haskellType received err =
  T.concat
    [ "Failed to parse Haskell type `",
      haskellType,
      "`, but received ",
      T.pack (show received),
      " | with error: ",
      err
    ]
