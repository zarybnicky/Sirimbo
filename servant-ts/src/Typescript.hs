{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Typescript where

import           Data.Proxy
import           Data.Text    (Text)
import qualified Data.Text    as T

import           Data.Tagged  (Tagged)
import           GHC.Generics

{-
   MASTER TYPE
-}
data TSIntermediate f =
    TSPrimitiveType TSPrimitive | TSCompositeType (TSComposite f)

{-
  Typescript Primitives. Have a default rep
-}
data TSPrimitive = TSNumber | TSString | TSBoolean | TSVoid
    deriving ( Eq, Show )

{-
  Composite Types
-}
data TSComposite f = TSCollectionRef (TSCollection f)
                   | TSOptionRef (TSOption f)
                   | TSStructuredType Text (TSStructured f)

newtype TSCollection f = TSCollection (TSIntermediate f)

newtype TSOption f = TSOption (TSIntermediate f)

newtype TSUnion f = TSUnion [TSIntermediate f]

instance Semigroup (TSUnion f) where
    (TSUnion l1) <> (TSUnion l2) = TSUnion $ l1 <> l2

{-
  Typescript "Data types", which are largely structural but can also be transformed into classes.
-}
data TSStructured f = TSRecordLike (TSRecord f) | TSUnionLike (TSUnion f)

newtype TSRecord f = TSRecord [TSField f]

data TSField f = TSField { fieldName :: FieldName
                         , fieldType :: TSIntermediate f
                         }

newtype FieldName = FieldName Text


class TypescriptType a where
    toTSIntermediate :: a -> TSIntermediate flavor
    default toTSIntermediate :: (Generic a, GenericTSIntermediate (Rep a))
            => a -> TSIntermediate flavor
    toTSIntermediate = genericToTS . from

instance {-# OVERLAPPABLE #-} (Generic a, GenericTSIntermediate (Rep a)) => TypescriptType a


{-| Helper typeclasses for intermediate translations
-}
class GenericTSIntermediate hkf where
    genericToTS :: hkf a -> TSIntermediate flavor

class GenericTSStructured hkf where
    toTSStructured :: hkf a -> TSStructured flavor

class GenericTSUnion hkf where
    toTSUnion :: hkf a -> TSUnion flavor

class GenericTSFields hkf where
    toTSFields :: hkf a -> [TSField flavor]

{-| Instances-}
instance (GenericTSFields f, GenericTSFields g)
    => GenericTSFields (f :*: g) where
    toTSFields _ = f1 <> f2
      where
        f1 = toTSFields (undefined :: f p)

        f2 = toTSFields (undefined :: g p)

instance (GenericTSIntermediate f, Selector s) => GenericTSFields (S1 s f) where
    toTSFields s = [ TSField { fieldName = FieldName $ T.pack $ selName s
                             , fieldType = genericToTS $ unM1 s
                             }
                   ]

instance (Datatype d, GenericTSFields f, GenericTSFields s)
    => GenericTSIntermediate (D1 d (C1 c (s :*: f))) where
    genericToTS d = TSCompositeType $ TSStructuredType typeName $ TSRecordLike $
        TSRecord $ toTSFields (unM1 . unM1 $ d)
      where
        typeName = T.pack (datatypeName d)

instance (Datatype d, GenericTSIntermediate f, Selector s)
    => GenericTSIntermediate (D1 d (C1 c (S1 s f))) where
    genericToTS d = TSCompositeType $ TSStructuredType typeName $ TSRecordLike $
        TSRecord $ toTSFields (unM1 . unM1 $ d)
      where
        typeName = T.pack (datatypeName d)

instance Datatype d => GenericTSIntermediate (D1 d (C1 c U1)) where
    genericToTS d = TSPrimitiveType TSVoid

instance (GenericTSIntermediate f1)
    => GenericTSUnion (C1 c1 (S1 ('MetaSel 'Nothing a b 'DecidedLazy) f1)) where
    toTSUnion _ = TSUnion [ genericToTS (undefined :: f1 p) ]

instance (GenericTSUnion f, GenericTSUnion g) => GenericTSUnion (f :+: g) where
    toTSUnion _ = u1 <> u2
      where
        u1 = toTSUnion (undefined :: f p)

        u2 = toTSUnion (undefined :: g p)

instance (Datatype d, GenericTSUnion s, GenericTSUnion f)
    => GenericTSIntermediate (D1 d (s :+: f)) where
    genericToTS datatype = TSCompositeType $ TSStructuredType typeName $
        TSUnionLike $ toTSUnion (unM1 datatype)
      where
        typeName = T.pack (datatypeName datatype)

instance (Datatype d, GenericTSIntermediate f1, GenericTSIntermediate f2)
    => GenericTSIntermediate (D1 d (C1 c1 (S1 ('MetaSel 'Nothing a b 'DecidedLazy) f1)
                                    :*: C1 c2 (S1 ('MetaSel 'Nothing a2 b2 'DecidedLazy) f2))) where
    genericToTS datatype = TSCompositeType $ TSStructuredType typeName $
        TSRecordLike $ TSRecord [ f1, f2 ]
      where
        typeName = T.pack (datatypeName datatype)

        f1 = TSField (FieldName "meow") (genericToTS (undefined :: f1 p))

        f2 = TSField (FieldName "f2") (genericToTS (undefined :: f2 p))

instance TypescriptType t => GenericTSIntermediate (Rec0 t) where
    genericToTS _ = toTSIntermediate (Proxy :: Proxy t)

instance (GenericTSStructured f, Constructor c)
    => GenericTSStructured (C1 c f) where
    toTSStructured c = toTSStructured (unM1 c)

instance (GenericTSIntermediate f, Selector s)
    => GenericTSStructured (S1 s f) where
    toTSStructured s = TSUnionLike $ TSUnion [ genericToTS (unM1 s) ]

instance (GenericTSIntermediate f, GenericTSIntermediate g)
    => GenericTSStructured (f :+: g) where
    toTSStructured _ = TSUnionLike $ TSUnion [ genericToTS (undefined :: f p)
                                             , genericToTS (undefined :: g p)
                                             ]

instance TypescriptType a => TypescriptType (Proxy a) where
    toTSIntermediate _ = toTSIntermediate (undefined :: a)

instance TypescriptType a => TypescriptType (Tagged t a) where
    toTSIntermediate _ = toTSIntermediate (undefined :: a)

instance TypescriptType t => TypescriptType (Maybe t) where
    toTSIntermediate _ = TSCompositeType $ TSOptionRef $
        TSOption (toTSIntermediate (Proxy :: Proxy t))

instance TypescriptType t => TypescriptType [t] where
    toTSIntermediate _ = TSCompositeType $ TSCollectionRef $
        TSCollection (toTSIntermediate (Proxy :: Proxy t))

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


{-|
  Instantiate this class for all types that will be printed out as Text.
-}
class IsForeignType t where
    toForeignType :: t -> ForeignType

{-|
  A type that represents a reference and a declaration.
  For example, the following two lines are __declarations__
  The first declaration for 'Foo' uses teh __refName__ of 'Bar' to refer to it in the field `sField`

> data Foo = Foo {sField    :: Bar}
> data Bar = Bar {someField :: Int}
-}
data ForeignType = ForeignType { refName     :: Text
                               , declaration :: Text
                               }
    deriving ( Generic, Show )

data TypescriptOutput = TypescriptOutput { typeOutput    :: ForeignType
                                         , mbRequiresLib :: Maybe TSLibrary
                                         }

class (IsForeignType t) => OutputsTypescript t where
    toTypescriptOutput :: t -> TypescriptOutput

newtype TSLibrary = TSLibrary Text

mkTypescriptOut :: (IsForeignType t) => Maybe TSLibrary -> t -> TypescriptOutput
mkTypescriptOut mbLib foreignType =
    TypescriptOutput (toForeignType foreignType) mbLib

selfRefForeign :: Text -> ForeignType
selfRefForeign ref = ForeignType ref ref


{- DEFAULT FOREIGN INSTANCES -}
instance IsForeignType (TSComposite f)
    => IsForeignType (TSIntermediate f) where
    toForeignType (TSPrimitiveType prim)      = toForeignType prim
    toForeignType (TSCompositeType composite) = toForeignType composite

instance IsForeignType TSPrimitive where
    toForeignType TSString  = selfRefForeign "string"
    toForeignType TSNumber  = selfRefForeign "number"
    toForeignType TSBoolean = selfRefForeign "boolean"
    toForeignType TSVoid    = selfRefForeign "void"

showField :: (IsForeignType (TSComposite f)) => TSField f -> Text
showField (TSField (FieldName fName) fType) = fName <> " : "
    <> (refName . toForeignType) fType

showFields :: (IsForeignType (TSComposite f)) => [TSField f] -> Text
showFields fields = T.intercalate "\n" $
    fmap (\f -> "  " <> showField f) fields

defaultForeignArray :: (IsForeignType (TSComposite f))
    => TSCollection f -> ForeignType
defaultForeignArray (TSCollection tsType') =
    ForeignType { refName     = "Array<" <> rep <> ">"
                , declaration = "Array<" <> rep <> ">"
                }
  where
    rep = refName . toForeignType $ tsType'

defaultForeignUnion :: (IsForeignType (TSComposite f))
    => Text -> TSUnion f -> ForeignType
defaultForeignUnion unionName (TSUnion tsTypes') =
    ForeignType { refName     = unionName
                , declaration = "type " <> unionName <> " = " <> ns
                }
  where
    ns = T.intercalate " | " $ fmap (refName . toForeignType) tsTypes'

defaultOption :: (IsForeignType (TSComposite f)) => TSOption f -> ForeignType
defaultOption (TSOption tsType') =
    selfRefForeign ((refName . toForeignType $ tsType') <> " | null ")

mkTSInterface :: (IsForeignType (TSComposite f))
    => Text -> TSRecord f -> ForeignType
mkTSInterface iName (TSRecord fields') =
    ForeignType { refName     = iName
                , declaration = "interface " <> iName <> " { \n"
                      <> showFields fields' <> "\n}"
                }


{-| Core function for outputting typescript

Simple example using the `Vanilla` flavor

-}
-- |
-- >>> import Typescript.Internal.Flavors.Vanilla
-- >>> declaration $ foreignTypescript (Proxy :: Proxy Vanilla) (Proxy :: Proxy Int)
-- "number"
foreignTypescript
    :: (TypescriptType hsType, IsForeignType (TSComposite flavor))
    => Proxy flavor -> Proxy hsType -> ForeignType
foreignTypescript pFlavor tsType' = toForeignType $ toTSFlavor pFlavor tsType'

mkTypescriptDeclaration
    :: (TypescriptType hsType, IsForeignType (TSComposite flavor))
    => Proxy flavor -> Proxy hsType -> Text
mkTypescriptDeclaration pFlavor tsType' =
    declaration $ foreignTypescript pFlavor tsType'

toTSFlavor :: (TypescriptType hsType)
    => Proxy flavor -> Proxy hsType -> TSIntermediate flavor
toTSFlavor _ = toTSIntermediate


data FpTs

instance IsForeignType (TSComposite FpTs) where
    toForeignType (TSCollectionRef tsCollection) =
        defaultForeignArray tsCollection
    toForeignType (TSOptionRef tsOption) = mkFpTSOption tsOption
    toForeignType (TSStructuredType typeName tsStructure) = case tsStructure of
        TSUnionLike tsUnion -> defaultForeignUnion typeName tsUnion
        TSRecordLike tsData -> mkTSInterface typeName tsData

mkFpTSOption :: (IsForeignType (TSComposite f)) => TSOption f -> ForeignType
mkFpTSOption (TSOption tsType') = selfRefForeign $ "Option<"
    <> (refName . toForeignType $ tsType') <> ">"

instance OutputsTypescript (TSComposite FpTs) where
    toTypescriptOutput = mkTypescriptOut (Just (TSLibrary "fp-ts"))


