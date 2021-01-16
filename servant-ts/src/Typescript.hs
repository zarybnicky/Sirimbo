{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Typescript
  ( TSIntermediate (..),
    TSPrimitive (..),
    TSComposite (..),
    TypescriptType (..),
    ForeignType (..),
    IsForeignType (..),
  )
where

import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

data TSIntermediate
  = TSPrimitiveType TSPrimitive
  | TSCompositeType TSComposite

data TSPrimitive = TSNumber | TSString | TSBoolean | TSVoid
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

instance Datatype d => GenericTSIntermediate (D1 d (C1 c U1)) where
  genericToTS d = TSPrimitiveType TSVoid

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
  toForeignType TSString = ForeignType "string" Nothing
  toForeignType TSNumber = ForeignType "number" Nothing
  toForeignType TSBoolean = ForeignType "boolean" Nothing
  toForeignType TSVoid = ForeignType "void" Nothing
