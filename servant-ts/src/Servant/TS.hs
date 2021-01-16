{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.TS where

import Control.Lens ((^.))
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc (Doc, Pretty (pretty), indent, vsep)
import GHC.Generics (Generic)
import Servant.API
import Servant.API.Flatten (Flat)
import Servant.API.WebSocket (WebSocket)
import Servant.Foreign
import Servant.Foreign.Inflections (camelCase)
import Typescript
import Web.Cookie (SetCookie)

data User = User
  { name :: Text,
    age :: Int,
    isAdmin :: Bool,
    hasMI :: Maybe Text
  }
  deriving (Generic)

type SimpleAPI =
  "user" :> Get '[JSON] [User]
    :<|> "user" :> Capture "userId" Int :> Get '[JSON] User
    :<|> "api" :> "tournament" :> "ws" :> WebSocket
    :<|> AuthProtect "php-session" :> "api" :> "tournament" :> "admin" :> "ws" :> WebSocket
    :<|> QrPaymentAPI
    :<|> AuthProtect "php-session" :> "logout"
      :> Verb
           'GET
           303
           '[JSON]
           (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)

data PNG

type QrPaymentAPI =
  "api" :> "qr-payment.png"
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

instance (HasForeignType l t Text, HasForeign l t sub) => HasForeign l t (AuthProtect sym :> sub) where
  type Foreign t (AuthProtect sym :> sub) = Foreign t sub
  foreignFor l _ _ req = foreignFor l Proxy (Proxy @sub) req

run :: IO ()
run = do
  let asTs = servantToReqTS (Proxy @(FilterAPI (Flat SimpleAPI)))
  T.putStrLn $ apiToTypeDeclarationDoc asTs
  T.putStrLn $ apiToFunctionDoc asTs

servantToReqTS ::
  ( HasForeign LangTs TSIntermediate api,
    GenerateList TSIntermediate (Foreign TSIntermediate api)
  ) =>
  Proxy api ->
  [Req TSIntermediate]
servantToReqTS = listFromAPI (Proxy @LangTs) Proxy

apiToFunctionDoc :: [Req TSIntermediate] -> Text
apiToFunctionDoc apiReqs = T.unlines (printTSFunction <$> apiReqs)

apiToTypeDeclarationDoc :: IsForeignType t => [Req t] -> Text
apiToTypeDeclarationDoc asTS =
  T.unlines . mapMaybe (declaration . toForeignType) $ mapMaybe _reqReturnType asTS

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
      "`);\n}"
    ]
  where
    segmentToCapture seg = case unSegment seg of
      Cap (Arg (PathSegment n) _) -> "${" <> n <> "}"
      Static (PathSegment ps) -> ps
    segmentToArg seg = case unSegment seg of
      Cap (Arg (PathSegment n) t) -> Just (n <> ": " <> refName (toForeignType t))
      _ -> Nothing
