{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Olymp
  ( makeServer
  , runServer
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Effect (Effs)
import Control.Effect.AtomicState (atomicGet)
import Control.Effect.Error (throw)
import Control.Monad.Except (runExceptT, forever, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson (FromJSON, Value(Array), encode, eitherDecode')
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy.Char8 as BCL
import Data.IORef (newIORef)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Pool (withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileThrow)
import Database.Persist.Sql (get, repsert)
import Database.Persist.MySQL (ConnectInfo(..), createMySQLPool, defaultConnectInfo)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
-- import Network.Wai.Middleware.Cors
import Olymp.Auth (PhpAuth, PhpAuthHandler, phpAuthHandler)
-- import Olymp.Cli (Args(..), parseArgs)
import Olymp.Effect.Database (query)
import Olymp.Effect.Log (WithLog, logInfo)
import Olymp.Monad (interpretServer, AppC, AppStack)
import Olymp.Schema (User(..), Key(ParameterKey), Parameter(..))
import Olymp.Tournament.API (initialTournament, tournamentSocket, tournamentAdminSocket)
import Olymp.Tournament.Base (Tournament, NodeId, withTournament, propagateWinners)
import Olymp.YouTube.Worker (main)
import Servant
import Servant.API.WebSocket (WebSocket)
-- import Servant.Multipart (MultipartData, MultipartForm, Tmp)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data AppConfig = AppConfig
  { dbHost :: String
  , dbUser :: String
  , dbPassword :: String
  , dbDatabase :: String
  } deriving (Show, Generic, FromJSON)

runServer :: IO ()
runServer = do
  port <- fromMaybe 4000 . (readMaybe =<<) <$> lookupEnv "PORT"
  run port =<< makeServer

makeServer :: IO Application
makeServer = do
  configFile <- fromMaybe "config.yaml" <$> lookupEnv "CONFIG"
  putStrLn $ "Reading config from: " <> configFile
  AppConfig{..} <- decodeFileThrow configFile
  let connectInfo = defaultConnectInfo
        { connectHost = dbHost
        , connectUser = dbUser
        , connectPassword = dbPassword
        , connectDatabase = dbDatabase
        }
  pool <- runStdoutLoggingT $ createMySQLPool connectInfo 5

  t <- withResource pool (runReaderT (get $ ParameterKey "tournament")) >>= \case
    Nothing -> pure initialTournament
    Just (Parameter tt) -> case eitherDecode' (BCL.pack $ T.unpack tt) of
      Left err -> putStrLn err >> pure initialTournament
      Right tt' -> pure tt'
  ref <- newIORef (withTournament propagateWinners t)
  ref' <- newIORef M.empty
  let runner :: forall a. AppC a -> Handler a
      runner = interpretServer ref ref' pool

  let saveState = runExceptT $ runHandler' $ runner $ do
        liftIO $ threadDelay 100000000
        state <- T.pack . BCL.unpack . encode <$> atomicGet @(Tournament NodeId)
        _ <- query $ repsert (ParameterKey "tournament") (Parameter state)
        pure ()
  _ <- forkIO (forever saveState)

  pure $ appServer runner
  where

    appServer :: (forall a. AppC a -> Handler a) -> Application
    appServer runner =
      -- cors (const $ Just simpleCorsResourcePolicy
      --       { corsRequestHeaders = ["Content-Type"]
      --       , corsMethods = "PUT" : simpleMethods
      --       }) $
      serveWithContext (Proxy @OlympApi) (phpAuthHandler runner :. EmptyContext) $
      hoistServerWithContext
        (Proxy @OlympApi)
        (Proxy @'[PhpAuthHandler])
        runner
        server

type OlympApi
  = "api" :> "whoami" :> PhpAuth :> Get '[PlainText, JSON] Text
  :<|> "api" :> "tournament" :> "ws" :> WebSocket
  :<|> "api" :> "tournament" :> PhpAuth :> "admin" :> "ws" :> WebSocket
  -- :<|> "api" :> "editor" :> EditorApi
  :<|> "wp" :> "v2" :> WordpressApi

server :: ServerT OlympApi AppC
server
  = whoAmI
  :<|> tournamentSocket
  :<|> tournamentAdminSocket
  -- :<|> editorServer
  :<|> wordpressServer

whoAmI :: WithLog m => User -> m Text
whoAmI u = do
  logInfo (userName u)
  pure (userName u <> " " <> userSurname u)

type WordpressApi
  = "types" :> Get '[JSON] (M.Map String Value)
  :<|> "types" :> Capture "type" String :> Get '[JSON] Value
  :<|> "pages" :> Capture "page" Int :> Get '[JSON] Value
  :<|> "pages" :> Capture "page" Int :> "autosaves" :> Get '[JSON] [Value]
  :<|> "themes" :> Get '[JSON] Value
  :<|> "taxonomies" :> Get '[JSON] Value
  :<|> "users" :> Get '[JSON] Value
  :<|> "users" :> "me" :> Get '[JSON] Value
  :<|> "blocks" :> Get '[JSON] [Value]
  :<|> "blocks" :> Verb 'OPTIONS 200 '[JSON] (Headers '[Header "Allow" String] ())
  :<|> "media" :> Verb 'OPTIONS 200 '[JSON] (Headers '[Header "Allow" String] ())

wordpressServer :: Effs AppStack m => ServerT WordpressApi m
wordpressServer
  = pure demoTypes
  :<|> (\typ -> maybe (throw err404) pure (M.lookup typ demoTypes))
  :<|> (\_page -> pure demoPost)
  :<|> (\_page -> pure [])
  :<|> pure demoThemes
  :<|> pure demoTaxonomies
  :<|> pure (Array $ pure demoUser)
  :<|> pure demoUser
  :<|> pure []
  :<|> pure (addHeader "GET,POST,PUT,DELETE" ())
  :<|> pure (addHeader "GET,POST,PUT,DELETE" ())

demoUser :: Value
demoUser = [aesonQQ|{
  "id": 1,
  "name": "Human Made",
  "url": "",
  "description": "",
  "link": "https://demo.wp-api.org/author/humanmade/",
  "slug": "humanmade",
  "avatar_urls": {
    "24": "https://secure.gravatar.com/avatar/83888eb8aea456e4322577f96b4dbaab?s=24&d=mm&r=g",
    "48": "https://secure.gravatar.com/avatar/83888eb8aea456e4322577f96b4dbaab?s=48&d=mm&r=g",
    "96": "https://secure.gravatar.com/avatar/83888eb8aea456e4322577f96b4dbaab?s=96&d=mm&r=g"
  },
  "meta": [],
  "_links": {
    "self": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/users/1" }],
    "collection": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/users" }]
  }
}|]

demoTaxonomies :: Value
demoTaxonomies = [aesonQQ|{
  "category": {
    "name": "Categories",
    "slug": "category",
    "description": "",
    "types": ["post"],
    "visibility": { "show_ui": false },
    "hierarchical": true,
    "rest_base": "categories",
    "_links": {
      "collection": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/taxonomies" }],
      "wp:items": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/categories" }],
      "curies": [{
        "name": "wp",
        "href": "https://api.w.org/{rel}",
        "templated": true
      }]
    }
  },
  "post_tag": {
    "name": "Tags",
    "slug": "post_tag",
    "description": "",
    "types": ["post"],
    "visibility": { "show_ui": false },
    "hierarchical": false,
    "rest_base": "tags",
    "_links": {
      "collection": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/taxonomies" }],
      "wp:items": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/tags" } ],
      "curies": [{
        "name": "wp",
        "href": "https://api.w.org/{rel}",
        "templated": true
      }]
    }
  }
}|]

demoThemes :: Value
demoThemes = [aesonQQ|[{
  "theme_supports": {
    "formats": [
      "standard",
      "aside",
      "image",
      "video",
      "quote",
      "link",
      "gallery",
      "audio"
    ],
    "post-thumbnails": true,
    "responsive-embeds": false
  }
}]|]

demoPost :: Value
demoPost = [aesonQQ|{
  "id": 1,
  "guid": "...",
  "status": "auto-draft",
  "type": "page",
  "title": "Test",
  "content": { "raw": "" }
}|]

demoTypes :: M.Map String Value
demoTypes = M.fromList
  [ ("post", typePost)
  , ("page", typePage)
  , ("attachment", typeAttachment)
  , ("wb_block", typeBlock)
  ]

typeBlock :: Value
typeBlock = [aesonQQ|{
  "labels": { "singular_name": "Block" },
  "description": "",
  "hierarchical": false,
  "viewable": false,
  "name": "Blocks",
  "slug": "wp_block",
  "supports": { "title": true, "editor": true },
  "taxonomies": [],
  "rest_base": "blocks",
  "_links": {
    "collection": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/types" } ],
    "wp:items": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/blocks" }],
    "curies": [{
      "name": "wp",
      "href": "https://api.w.org/{rel}",
      "templated": true
    }]
  },
  "headers": []
}|]

typeAttachment :: Value
typeAttachment = [aesonQQ|{
  "labels": { "singular_name": "Attachment" },
  "description": "",
  "hierarchical": false,
  "viewable": true,
  "name": "Media",
  "slug": "attachment",
  "supports": {
    "title": true,
    "author": true,
    "comments": true
  },
  "taxonomies": [],
  "rest_base": "media",
  "_links": {
    "collection": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/types" } ],
    "wp:items": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/media" }],
    "curies": [{
      "name": "wp",
      "href": "https://api.w.org/{rel}",
      "templated": true
    }]
  }
}|]

typePage :: Value
typePage = [aesonQQ|{
  "labels": { "singular_name": "Page" },
  "description": "",
  "hierarchical": true,
  "viewable": true,
  "name": "Pages",
  "slug": "page",
  "supports": {
    "author": false,
    "comments": false,
    "custom-fields": false,
    "discussion": false,
    "editor": true,
    "excerpt": true,
    "page-attributes": false,
    "revisions": false,
    "thumbnail": false,
    "title": false
  },
  "taxonomies": [],
  "rest_base": "pages",
  "_links": {
    "collection": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/types" } ],
    "wp:items": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/pages" }],
    "curies": [{
      "name": "wp",
      "href": "https://api.w.org/{rel}",
      "templated": true
    }]
  }
}|]

typePost :: Value
typePost = [aesonQQ|{
  "labels": { "singular_name": "Post" },
  "description": "",
  "hierarchical": false,
  "viewable": true,
  "name": "Posts",
  "slug": "post",
  "supports": {
    "author": false,
    "comments": false,
    "custom-fields": true,
    "editor": true,
    "excerpt": false,
    "page-attributes": false,
    "revisions": false,
    "thumbnail": false,
    "title": true,
    "post-formats": true
  },
  "taxonomies": ["category", "post_tag"],
  "rest_base": "posts",
  "_links": {
    "collection": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/types" } ],
    "wp:items": [{ "href": "https://demo.wp-api.org/wp-json/wp/v2/posts" }],
    "curies": [{
      "name": "wp",
      "href": "https://api.w.org/{rel}",
      "templated": true
    }]
  }
}|]
