{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Olymp
  ( makeApplication
  , parseArgs
  , Warp.run
  ) where

import Control.Monad.Except (ExceptT(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import Polysemy (Sem, runM)
import Polysemy.Error (runError)
import Servant
import Servant.Server
  (Application, Handler(..), err404, errBody, hoistServer, serve)

import Olymp.Cli (Args(..), parseArgs)
import Olymp.Log

makeApplication :: Args -> IO (Int, Application)
makeApplication args = do
  let app = serve api $ hoistServer api interpretServer server
  pure (port args, app)
  where
    api = Proxy @TodoAPI
    interpretServer =
      Handler . ExceptT . (first handleErrors <$>) . runM .
      runLogToStdout .
      runError @TodoError
    handleErrors (TodoNotAvailable _) = err404 {errBody = "Todo does not exist"}

data TodoError = TodoNotAvailable Int

data Todo = Todo
  { _title :: String
  , _completed :: Bool
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

type TodoAPI
  = "todo" :> (Get '[JSON] (M.Map Int Todo) :<|>
               Capture "id" Int :> Get '[JSON] Todo :<|>
               Capture "id" Int :> "toggle" :> Get '[JSON] Todo :<|>
               ReqBody '[JSON] Todo :> Post '[JSON] Todo)

server :: WithLog r => ServerT TodoAPI (Sem r)
server =
  (logInfo "get" >> pure M.empty) :<|>
  (\_ -> pure (Todo "" False)) :<|>
  (\_ -> pure (Todo "" False)) :<|>
  (\_ -> pure (Todo "" False))

