{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.API
  ( OlympAPI,
    olympAPI,
  )
where

import Control.Effect (Eff)
import qualified Data.Aeson as A
import Data.Time (Day (ModifiedJulianDay), UTCTime (..))
import Database.Persist.Sql (fromSqlKey)
import Olymp.Effect (AppStack)
import Olymp.Effect.Session (SessionEff, deleteSession)
import Olymp.Prelude
import Olymp.Schema (Permission (..), User (..))
import Olymp.Tournament.API (tournamentAdminSocket, tournamentSocket)
import Servant
import Servant.API.WebSocket (WebSocket)
import Web.Cookie (SetCookie (..), defaultSetCookie)

type OlympAPI =
  PhpMaybeAuth :> "api" :> "graphql-auth" :> Get '[JSON] A.Value
    :<|> PhpAuth :> "logout" :> Verb 'GET 303 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)
    :<|> "api" :> "tournament" :> "ws" :> WebSocket
    :<|> "api" :> "admin" :> "ws" :> PhpAuth :> WebSocket

olympAPI :: Effs AppStack m => ServerT OlympAPI m
olympAPI =
  graphqlAuth
    :<|> logout
    :<|> tournamentSocket
    :<|> tournamentAdminSocket

graphqlAuth :: Eff Database m => Maybe (SessionId, Entity User) -> m A.Value
graphqlAuth = \case
  Nothing -> pure $ A.object ["X-Hasura-Role" A..= ("anonymous" :: String)]
  Just (_, eUser) -> pure . A.object $
    [ "X-Hasura-Role" A..= case fromSqlKey $ userPermission (entityVal eUser) of
        0 -> "anonymous" :: String
        1 -> "admin"
        _ -> "member",
      "X-Hasura-User-Id" A..= show (fromSqlKey (entityKey eUser))
    ]

logout ::
  Eff SessionEff m =>
  (SessionId, Entity User) ->
  m (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)
logout (sid, _) = do
  deleteSession sid
  pure $ addHeader sessionCookie (addHeader "/" NoContent)
  where
    sessionCookie =
      defaultSetCookie
        { setCookieName = "PHPSESSID",
          setCookieValue = "",
          setCookiePath = Just "/",
          setCookieExpires = Just (UTCTime (ModifiedJulianDay 50000) 0)
        }
