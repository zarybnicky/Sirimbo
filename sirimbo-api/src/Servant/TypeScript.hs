{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.TypeScript (FilterAPI, apiToTypeScript) where

import Control.Lens ((^.))
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Servant.API
import Servant.API.WebSocket (WebSocket)
import Servant.Foreign
import Servant.JuicyPixels (PNG)
import Control.Monad ((<=<))
import Data.Aeson (Value)

type family FilterAPI api where
  FilterAPI (Stream method status framing ct a) = EmptyAPI
  FilterAPI (StreamBody' mods framing ct a :> b) = EmptyAPI
  FilterAPI (Verb method statusCode '[PNG] a) = EmptyAPI
  FilterAPI (Verb 'OPTIONS statusCode types a) = EmptyAPI
  FilterAPI (Verb method statusCode content (Headers _ a)) = Verb method statusCode content a
  FilterAPI (reqBody '[PNG] a :> b) = EmptyAPI
  FilterAPI WebSocket = EmptyAPI
  FilterAPI (a :<|> b) = FilterAPI a :<|> FilterAPI b
  FilterAPI ((a :: k) :> b) = a :> FilterAPI b
  FilterAPI a = a

instance (HasForeign l t sub) => HasForeign l t (AuthProtect sym :> sub) where
  type Foreign t (AuthProtect sym :> sub) = Foreign t sub
  foreignFor l _ _ req = foreignFor l Proxy (Proxy @sub) req

apiToTypeScript ::
  ( HasForeign LangTs TSIntermediate api,
    GenerateList TSIntermediate (Foreign TSIntermediate api)
  ) =>
  Proxy api ->
  Text
apiToTypeScript p = T.unlines $ mconcat
  [ printTSFunction <$> reqs,
    [],
    mapMaybe (declaration . toForeignType <=< _reqReturnType) reqs
  ]
  where reqs = listFromAPI (Proxy @LangTs) Proxy p

data LangTs

instance TypescriptType a => HasForeignType LangTs TSIntermediate a where
  typeFor _ _ _ = toTSIntermediate (Proxy @a)

printTSFunction :: Req TSIntermediate -> Text
printTSFunction req =
  T.concat
    [ "function ",
      camelCase (req ^. reqFuncName),
      "(",
      T.intercalate "," $ mapMaybe segmentToArg (req ^. reqUrl . path),
      "): ",
      case req ^. reqReturnType of
        Nothing -> "void"
        Just ret -> "Promise<" <> (refName . toForeignType) ret <> ">",
      " {\n  return fetch(`/",
      T.intercalate "/" (segmentToCapture <$> req ^. reqUrl . path),
      "`).then(x => x.json());\n}"
    ]
  where
    segmentToCapture seg = case unSegment seg of
      Cap (Arg (PathSegment n) _) -> "${" <> n <> "}"
      Static (PathSegment ps) -> ps
    segmentToArg seg = case unSegment seg of
      Cap (Arg (PathSegment n) t) -> Just (n <> ": " <> refName (toForeignType t))
      _ -> Nothing


{- TypeScript definitions -}


data TSIntermediate
  = TSPrimitiveType TSPrimitive
  | TSCompositeType TSComposite

data TSPrimitive = TSNumber | TSString | TSBoolean | TSVoid | TSObject
  deriving (Eq, Show)

data TSComposite
  = TSCollection TSIntermediate
  | TSOption TSIntermediate
  | TSRecord Text [(Text, TSIntermediate)]
  | TSUnion Text [TSIntermediate]

class TypescriptType a where
  toTSIntermediate :: a -> TSIntermediate

instance {-# OVERLAPPABLE #-} (Generic a, GenericTSIntermediate (Rep a)) => TypescriptType a where
  toTSIntermediate = genericToTS . from

class GenericTSIntermediate hkf where
  genericToTS :: hkf a -> TSIntermediate

instance (Datatype d, GenericTSFields f, GenericTSFields s) => GenericTSIntermediate (D1 d (C1 c (s :*: f))) where
  genericToTS d = TSCompositeType $ TSRecord (T.pack $ datatypeName d) (toTSFields . unM1 . unM1 $ d)

instance (Datatype d, GenericTSIntermediate f, Selector s) => GenericTSIntermediate (D1 d (C1 c (S1 s f))) where
  genericToTS d = TSCompositeType $ TSRecord (T.pack $ datatypeName d) (toTSFields . unM1 . unM1 $ d)

instance GenericTSIntermediate (D1 d (C1 c U1)) where
  genericToTS _ = TSPrimitiveType TSVoid

instance (Datatype d, GenericTSUnion s, GenericTSUnion f) => GenericTSIntermediate (D1 d (s :+: f)) where
  genericToTS d = TSCompositeType $ TSUnion (T.pack $ datatypeName d) (toTSUnion $ unM1 d)

instance TypescriptType t => GenericTSIntermediate (Rec0 t) where
  genericToTS _ = toTSIntermediate (Proxy :: Proxy t)

class GenericTSUnion hkf where
  toTSUnion :: hkf a -> [TSIntermediate]

instance GenericTSIntermediate f1 => GenericTSUnion (C1 c1 (S1 ('MetaSel 'Nothing a b 'DecidedLazy) f1)) where
  toTSUnion _ = [genericToTS (undefined :: f1 p)]

instance (GenericTSUnion f, GenericTSUnion g) => GenericTSUnion (f :+: g) where
  toTSUnion _ = toTSUnion (undefined :: f p) <> toTSUnion (undefined :: g p)

class GenericTSFields hkf where
  toTSFields :: hkf a -> [(Text, TSIntermediate)]

instance (GenericTSFields f, GenericTSFields g) => GenericTSFields (f :*: g) where
  toTSFields _ = toTSFields (undefined :: f p) <> toTSFields (undefined :: g p)

instance (GenericTSIntermediate f, Selector s) => GenericTSFields (S1 s f) where
  toTSFields s = [(T.pack (selName s), genericToTS (unM1 s))]

instance TypescriptType a => TypescriptType (Proxy a) where
  toTSIntermediate _ = toTSIntermediate (undefined :: a)

instance TypescriptType a => TypescriptType (Tagged t a) where
  toTSIntermediate _ = toTSIntermediate (undefined :: a)

instance TypescriptType t => TypescriptType (Maybe t) where
  toTSIntermediate _ = TSCompositeType $ TSOption (toTSIntermediate (Proxy :: Proxy t))

instance TypescriptType t => TypescriptType [t] where
  toTSIntermediate _ = TSCompositeType $ TSCollection (toTSIntermediate (Proxy :: Proxy t))

instance TypescriptType Text where
  toTSIntermediate _ = TSPrimitiveType TSString

instance TypescriptType Int where
  toTSIntermediate _ = TSPrimitiveType TSNumber

instance TypescriptType Double where
  toTSIntermediate _ = TSPrimitiveType TSNumber

instance TypescriptType Float where
  toTSIntermediate _ = TSPrimitiveType TSNumber

instance TypescriptType Bool where
  toTSIntermediate _ = TSPrimitiveType TSBoolean

instance TypescriptType Value where
  toTSIntermediate _ = TSPrimitiveType TSObject

instance TypescriptType () where
  toTSIntermediate _ = TSPrimitiveType TSVoid

data ForeignType = ForeignType
  { refName :: Text,
    declaration :: Maybe Text
  }
  deriving (Generic, Show)

class IsForeignType t where
  toForeignType :: t -> ForeignType

instance IsForeignType TSIntermediate where
  toForeignType (TSPrimitiveType t) = toForeignType t
  toForeignType (TSCompositeType t) = toForeignType t

instance IsForeignType TSComposite where
  toForeignType (TSCollection t) =
    ForeignType ("Array<" <> refName (toForeignType t) <> ">") Nothing
  toForeignType (TSOption t) =
    ForeignType (refName (toForeignType t) <> " | null ") Nothing
  toForeignType (TSRecord t fields) =
    ForeignType t (Just ("interface " <> t <> " { \n" <> fs <> "\n}"))
    where
      mkField (n, f) = "  " <> n <> ": " <> refName (toForeignType f)
      fs = T.intercalate "\n" (mkField <$> fields)
  toForeignType (TSUnion t types) =
    ForeignType t (Just ("type " <> t <> " = " <> ns))
    where
      ns = T.intercalate " | " $ refName . toForeignType <$> types

instance IsForeignType TSPrimitive where
  toForeignType = \case
    TSString -> ForeignType "string" Nothing
    TSNumber -> ForeignType "number" Nothing
    TSBoolean -> ForeignType "boolean" Nothing
    TSVoid -> ForeignType "void" Nothing
    TSObject -> ForeignType "object" Nothing
