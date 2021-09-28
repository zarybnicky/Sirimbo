{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
    generateTs,
  )
where

import Control.Effect (Effs)
import qualified Data.Text.IO as T
import Network.HTTP.Client (Manager)
import Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..), defaultOnExc, waiProxyTo)
import Olymp.Auth (PhpAuthHandler, phpAuthHandler)
import Olymp.API (OlympAPI, olympAPI)
import Olymp.Effect (AppStack, interpretServer)
import Servant
import Servant.API.Flatten (Flat)
import Servant.TypeScript (FilterAPI, apiToTypeScript)

olympServer :: Effs AppStack m => Int -> Manager -> (forall a. m a -> Handler a) -> Application
olympServer proxyPort manager runner =
  -- cors (const $ Just simpleCorsResourcePolicy
  --       { corsRequestHeaders = ["Content-Type"]
  --       , corsMethods = "PUT" : simpleMethods
  --       })
  serveWithContext (Proxy @(OlympAPI :<|> Raw)) (phpAuthHandler runner :. EmptyContext) $
    api :<|> Tagged phpProxy
  where
    api = hoistServerWithContext (Proxy @OlympAPI) (Proxy @'[PhpAuthHandler]) runner olympAPI
    phpProxy = waiProxyTo forwardRequest defaultOnExc manager
    forwardRequest _ = pure $ WPRProxyDest (ProxyDest "127.0.0.1" proxyPort)

generateTs :: IO ()
generateTs = T.putStrLn (apiToTypeScript (Proxy @(FilterAPI (Flat OlympAPI))))
