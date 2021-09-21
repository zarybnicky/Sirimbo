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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.TypeScript (FilterAPI, apiToTypeScript) where

import Control.Lens ((^.))
import Control.Monad ((<=<))
import Data.Aeson (Value)
import Data.Char (toUpper)
import Data.Int (Int64)
import Data.Time (Day, UTCTime)
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
apiToTypeScript p =
  T.unlines $
    mconcat
      [ printTSFunction <$> reqs,
        [""],
        mapMaybe (declaration . toForeignType <=< _reqReturnType) reqs
      ]
  where
    reqs = listFromAPI (Proxy @LangTs) Proxy p

data LangTs

instance TypescriptType a => HasForeignType LangTs TSIntermediate a where
  typeFor _ _ _ = toTSIntermediate (Proxy @a)

printTSFunction :: Req TSIntermediate -> Text
printTSFunction req =
  T.concat
    [ "function ",
      T.concat $ functionName (req ^. reqFuncName),
      "(",
      T.intercalate "," $ mapMaybe segmentToArg (req ^. reqUrl . path),
      "): ",
      returnType (req ^. reqReturnType),
      " {\n  return fetch(`/",
      T.intercalate "/" (segmentToCapture <$> req ^. reqUrl . path),
      "`).then(x => x.json());\n}"
    ]
  where
    functionName (FunctionName n) =
      case n of
        [] -> []
        p : ps -> (p :) . flip map (T.splitOn "-" =<< ps) $ \x ->
          case T.uncons x of
            Nothing -> ""
            Just (c, cs) -> T.cons (toUpper c) cs
    returnType t = "Promise<" <> maybe "void" (refName . toForeignType) t <> ">"
    segmentToCapture (Segment seg) = case seg of
      Cap (Arg (PathSegment n) _) -> "${" <> n <> "}"
      Static (PathSegment ps) -> ps
    segmentToArg (Segment seg) = case seg of
      Cap (Arg (PathSegment n) t) -> Just (n <> ": " <> refName (toForeignType t))
      _ -> Nothing

{- TypeScript definitions -}

data TSIntermediate
  = TSNumber
  | TSString
  | TSBoolean
  | TSVoid
  | TSObject
  | TSCollection TSIntermediate
  | TSOption TSIntermediate
  | TSRecord Text [(Text, TSIntermediate)]
  | TSUnion Text [TSIntermediate]
  deriving (Eq, Show)

class TypescriptType a where
  toTSIntermediate :: a -> TSIntermediate

instance {-# OVERLAPPABLE #-} (Generic a, GenericTSIntermediate (Rep a)) => TypescriptType a where
  toTSIntermediate = genericToTS . from

class GenericTSIntermediate hkf where
  genericToTS :: hkf a -> TSIntermediate

instance (Datatype d, GenericTSFields f, GenericTSFields s) => GenericTSIntermediate (D1 d (C1 c (s :*: f))) where
  genericToTS d = TSRecord (T.pack $ datatypeName d) (toTSFields . unM1 . unM1 $ d)

instance (Datatype d, GenericTSIntermediate f, Selector s) => GenericTSIntermediate (D1 d (C1 c (S1 s f))) where
  genericToTS d = TSRecord (T.pack $ datatypeName d) (toTSFields . unM1 . unM1 $ d)

instance GenericTSIntermediate (D1 d (C1 c U1)) where
  genericToTS _ = TSVoid

instance (Datatype d, GenericTSUnion s, GenericTSUnion f) => GenericTSIntermediate (D1 d (s :+: f)) where
  genericToTS d = TSUnion (T.pack $ datatypeName d) (toTSUnion $ unM1 d)

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
  toTSIntermediate _ = TSOption (toTSIntermediate (Proxy :: Proxy t))

instance TypescriptType t => TypescriptType [t] where
  toTSIntermediate _ = TSCollection (toTSIntermediate (Proxy :: Proxy t))

instance TypescriptType Text where
  toTSIntermediate _ = TSString

instance TypescriptType Int where
  toTSIntermediate _ = TSNumber

instance TypescriptType Int64 where
  toTSIntermediate _ = TSNumber

instance TypescriptType Double where
  toTSIntermediate _ = TSNumber

instance TypescriptType Day where
  toTSIntermediate _ = TSString

instance TypescriptType UTCTime where
  toTSIntermediate _ = TSString

instance TypescriptType Float where
  toTSIntermediate _ = TSNumber

instance TypescriptType Bool where
  toTSIntermediate _ = TSBoolean

instance TypescriptType Value where
  toTSIntermediate _ = TSObject

instance TypescriptType () where
  toTSIntermediate _ = TSVoid

data ForeignType = ForeignType
  { refName :: Text,
    declaration :: Maybe Text
  }
  deriving (Generic, Show)

class IsForeignType t where
  toForeignType :: t -> ForeignType

instance IsForeignType TSIntermediate where
  toForeignType = \case
    TSString -> ForeignType "string" Nothing
    TSNumber -> ForeignType "number" Nothing
    TSBoolean -> ForeignType "boolean" Nothing
    TSVoid -> ForeignType "void" Nothing
    TSObject -> ForeignType "object" Nothing
    TSCollection (toForeignType -> t) ->
      ForeignType ("Array<" <> refName t <> ">") (declaration t)
    TSOption (toForeignType -> t) ->
      ForeignType (refName t <> " | null ") (declaration t)
    TSRecord t fs ->
      ForeignType t (Just ("interface " <> t <> " { \n" <> mkFields fs <> "\n}"))
    TSUnion t cs ->
      ForeignType t (Just ("type " <> t <> " = " <> mkUnion cs))
    where
      mkFields :: [(Text, TSIntermediate)] -> Text
      mkFields = T.intercalate "\n" . fmap (\(n, f) -> "  " <> n <> ": " <> refName (toForeignType f))
      mkUnion :: [TSIntermediate] -> Text
      mkUnion = T.intercalate " | " . fmap (refName . toForeignType)
