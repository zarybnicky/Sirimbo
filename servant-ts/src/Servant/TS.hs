{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.TS where

import           Control.Lens
import           Data.Maybe
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Tagged                 (Tagged)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Prettyprint.Doc
import           GHC.Generics
import           Servant.API
import           Servant.API.Flatten         (Flat)
import           Servant.API.WebSocket       (WebSocket)
import           Servant.Foreign
import           Servant.Foreign.Inflections (camelCase)
import           Typescript
import           Web.Cookie                  (SetCookie)


data User = User
  { name    :: Text
  , age     :: Int
  , isAdmin :: Bool
  , hasMI   :: Maybe Text
  } deriving (Generic)

type SimpleAPI
  = "user" :> Get '[JSON] [User]
  :<|> "user" :> Capture "userId" Int :> Get '[JSON] User
  :<|> "api" :> "tournament" :> "ws" :> WebSocket
  :<|> AuthProtect "php-session" :> "api" :> "tournament" :> "admin" :> "ws" :> WebSocket
  :<|> QrPaymentAPI
  :<|> AuthProtect "php-session" :> "logout" :> Verb 'GET 303 '[JSON]
    (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)

data PNG

type QrPaymentAPI
  = "api" :> "qr-payment.png"
  :> QueryParam "acc" (Tagged (Proxy "ACC") Text)
  :> QueryParam "am" (Tagged (Proxy "AM") Text)
  :> QueryParam "msg" (Tagged (Proxy "MSG") Text)
  :> QueryParam "ss" (Tagged (Proxy "X-SS") Text)
  :> QueryParam "vs" (Tagged (Proxy "X-VS") Text)
  :> QueryParam "ks" (Tagged (Proxy "X-KS") Text)
  :> Get '[PNG] ()

type family FilterAPI api where
  FilterAPI (Stream method status framing ct a) = EmptyAPI
  FilterAPI (StreamBody' mods framing ct a :> b) = EmptyAPI
  FilterAPI (Verb method statusCode '[PNG] a) = EmptyAPI
  FilterAPI (Verb method statusCode content (Headers _ a)) = Verb method statusCode content a
  FilterAPI (reqBody '[PNG] a :> b) = EmptyAPI
  FilterAPI WebSocket = EmptyAPI
  FilterAPI (a :<|> b) = FilterAPI a :<|> FilterAPI b
  FilterAPI ((a :: k) :> b) = a :> FilterAPI b
  FilterAPI a = a

instance (HasForeignType l t Text, HasForeign l t sub)
    => HasForeign l t (AuthProtect sym :> sub) where
    type Foreign t (AuthProtect sym :> sub) = Foreign t sub
    foreignFor l _ _ req = foreignFor l Proxy (Proxy @sub) req

run :: IO ()
run = do
  let asTs = servantToReqTS (Proxy @FpTs) (Proxy @(FilterAPI (Flat SimpleAPI)))
  print $ apiToFunctionDoc asTs defaultReqToTSFunction
  print $ apiToTypeDeclarationDoc asTs


data LangTypescript

instance (TypescriptType a)
    => HasForeignType LangTypescript (TSIntermediate flavor) a where
    typeFor _ _ _ = toTSIntermediate (Proxy :: Proxy a)

servantToReqTS
    :: ( HasForeign LangTypescript (TSIntermediate flavor) api
       , GenerateList (TSIntermediate flavor) (Foreign (TSIntermediate flavor) api)
       )
    => Proxy flavor
    -> Proxy api
    -> [Req (TSIntermediate flavor)]
servantToReqTS _ = listFromAPI (Proxy :: Proxy LangTypescript)
                               (Proxy :: Proxy (TSIntermediate flavor))

{- Represents the essential parts to describe a Typescript function -}
data TSFunctionConfig =
  TSFunctionConfig
    {_tsFuncName   :: Text
    ,_tsArgs       :: [TSArg]
    ,_tsReturnType :: Text
    ,_body         :: TSFunctionBody
    }

{- The main function to use when wanting to print out all the desired network request calls -}
printTSFunction :: TSFunctionConfig -> Text
printTSFunction (TSFunctionConfig tsFuncName' tsArgs' tsReturnType' body') =
  T.pack $ show $ vsep [pretty funcDeclareLine, printBody, pretty ("}" :: Text)]
 where
  funcDeclareLine =
    "function "
      <> tsFuncName'
      <> "("
      <> printArgs tsArgs'
      <> "): "
      <> tsReturnType'
      <> " {"
  printBody = indent 2 $ vsep (pretty <$> getTSFunctionBody body')


newtype TSFunctionBody = TSFunctionBody {getTSFunctionBody :: Lines}
type Lines = [Text]

{-|TSArg is an argument in a typescript function, consisting of a name and a type
  e.g. :  function getUserId(userId: number) {return userId}
  has _tsArgs = [TSArg (TSTypedVar {varName = "userId", varType: "number"})]
-}
newtype TSArg = TSArg {getTypedVar :: TSTypedVar} deriving (Show)
data TSTypedVar = TSTypedVar {varName :: Text, varType :: Text} deriving (Show)



{-| The below functions are generally considered for internal use only, but the module exposes them in case of edge case customization
-}
printArgs :: [TSArg] -> Text
printArgs tsArgs' =
  T.intercalate "," $ printTSTypedVar . getTypedVar <$> tsArgs'

printTSTypedVar :: TSTypedVar -> Text
printTSTypedVar (TSTypedVar name' type') = name' <> " : " <> type'

reqToTSFunctionArgs
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> [TSArg]
reqToTSFunctionArgs req =
  map (reqArgToTSArg . captureArg) . filter isCapture $ req ^. reqUrl . path

getReqUrl
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> Text
getReqUrl req = T.intercalate "/" (fmap handleSegment $ req ^. reqUrl . path)
 where
  handleSegment
    :: (IsForeignType (TSIntermediate flavor))
    => Segment (TSIntermediate flavor)
    -> Text
  handleSegment seg = case unSegment seg of
    Static (PathSegment ps) -> ps
    Cap arg -> "${" <> (varName . getTypedVar . reqArgToTSArg) arg <> "}"


reqArgToTSArg
  :: (IsForeignType (TSIntermediate flavor))
  => Arg (TSIntermediate flavor)
  -> TSArg
reqArgToTSArg (Arg argName' argType') = TSArg $ TSTypedVar
  { varName = unPathSegment argName'
  , varType = refName . toForeignType $ argType'
  }

reqToTSFunctionName
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> Text
reqToTSFunctionName req = camelCase $ req ^. reqFuncName

withDefaultUrlFunc :: Text -> Text
withDefaultUrlFunc t = "withRemoteBaseUrl(`" <> t <> "`)"

type TSBaseUrlMethod = Text -> Text

mkDefaultBody
  :: forall flavor trm
   . IsForeignType (TSIntermediate flavor)
  => Req (TSIntermediate flavor)
  -> TSBaseUrlMethod
  -> TSFunctionBody
mkDefaultBody req tsBaseUrlFunc = TSFunctionBody
  [ "return "
    <> "fetch("
    <> (tsBaseUrlFunc $ getReqUrl req)
    <> ")"
  ]

getTSReqMethodReturnType :: forall ft. (IsForeignType ft) => ft -> Text
getTSReqMethodReturnType t = "Promise<" <> (refName . toForeignType) t <> ">"

defaultReqToTSFunction
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> TSFunctionConfig
defaultReqToTSFunction req = TSFunctionConfig
  { _tsFuncName   = reqToTSFunctionName req
  , _tsArgs       = reqToTSFunctionArgs req
  , _tsReturnType = maybe "void" getTSReqMethodReturnType (req ^. reqReturnType)
  , _body         = mkDefaultBody req withDefaultUrlFunc
  }

data OutputFileNames = OutputFileNames { decFileLoc  :: FilePath
                                       , funcFileLoc :: FilePath
                                       }

apiToTSDocs :: (IsForeignType (TSIntermediate flavor))
            => [Req (TSIntermediate flavor)]
            -> (Req (TSIntermediate flavor) -> TSFunctionConfig)
            -> OutputFileNames
            -> IO ()
apiToTSDocs apiReqs reqToTSFunction' (OutputFileNames typeDecLoc'
                                                      funcFileLoc') = do
    writeFile funcFileLoc' $ show $ apiToFunctionDoc apiReqs reqToTSFunction'
    writeFile typeDecLoc' $ show $ apiToTypeDeclarationDoc apiReqs

{-| Utility methods for writing the function file
-}
apiToFunctionDoc :: (IsForeignType (TSIntermediate flavor))
                 => [Req (TSIntermediate flavor)]
                 -> (Req (TSIntermediate flavor) -> TSFunctionConfig)
                 -> Doc ann
apiToFunctionDoc apiReqs reqToTSFunction' =
    mkFunctionDoc $ fmap reqToTSFunction' apiReqs

{-| Utility methods for writing the TYPE DECLARATION file
-}
mkFunctionDoc :: [TSFunctionConfig]
              -> Doc ann
mkFunctionDoc tsFunctions = vsep $ fmap (pretty . printTSFunction) tsFunctions

apiToTypeDeclarationDoc :: IsForeignType t
                        => [Req t]
                        -> Doc ann
apiToTypeDeclarationDoc asTS = vsep $
    fmap (pretty . declaration . toForeignType) (mapMaybe _reqReturnType asTS)
