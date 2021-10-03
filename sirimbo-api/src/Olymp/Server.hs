{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Server
  ( AppStack,
    OlympAPI,
    interpretServer,
    olympServer,
  )
where

import Control.Effect (Effs)
import Data.Csv (Only)
import Data.OpenApi (OpenApi, ToSchema(..), ToParamSchema, binarySchema, NamedSchema(..))
import qualified Data.Text.IO as T
import Network.HTTP.Client (Manager)
import Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..), defaultOnExc, waiProxyTo)
import Olymp.Auth (PhpAuthHandler, phpAuthHandler, PhpAuth)
import Olymp.API (OlympAPI, olympAPI)
import Olymp.Effect (AppStack, interpretServer)
import Servant
import Servant.API.Flatten (Flat)
import Servant.OpenApi (HasOpenApi(..))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Data.Text (Text)
import Codec.Picture (Image)
import Data.Typeable (Typeable)
import Servant.API.WebSocket (WebSocket)

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

instance HasOpenApi a => HasOpenApi (PhpAuth :> a) where
  toOpenApi _ = toOpenApi @a Proxy
instance ToParamSchema (Tagged a Text)

instance ToSchema a => ToSchema (Only a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

instance Typeable a => ToSchema (Image a) where
  declareNamedSchema _ = pure $ NamedSchema (Just "Image") binarySchema

instance HasOpenApi WebSocket where
  toOpenApi _ = mempty

swaggerAPI :: OpenApi
swaggerAPI = toOpenApi (Proxy @OlympAPI)

olympServer :: Effs AppStack m => Int -> Manager -> (forall a. m a -> Handler a) -> Application
olympServer proxyPort manager runner =
  -- cors (const $ Just simpleCorsResourcePolicy
  --       { corsRequestHeaders = ["Content-Type"]
  --       , corsMethods = "PUT" : simpleMethods
  --       })
  serveWithContext (Proxy @(OlympAPI :<|> SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> Raw)) (phpAuthHandler runner :. EmptyContext) $
    api :<|> swaggerSchemaUIServer swaggerAPI :<|> Tagged phpProxy
  where
    api = hoistServerWithContext (Proxy @OlympAPI) (Proxy @'[PhpAuthHandler]) runner olympAPI
    phpProxy = waiProxyTo forwardRequest defaultOnExc manager
    forwardRequest _ = pure $ WPRProxyDest (ProxyDest "127.0.0.1" proxyPort)
