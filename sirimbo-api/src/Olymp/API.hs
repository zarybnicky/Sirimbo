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
  Just (_, eUser) -> do
    let permGroupKey = userPermission (entityVal eUser)
    permGroup <- query (get permGroupKey)
    pure . A.object $
      [ "X-Hasura-Role" A..= case fromSqlKey permGroupKey of
          0 -> "anonymous" :: String
          1 -> "admin"
          4 -> "admin"
          5 -> "admin"
          6 -> "admin"
          8 -> "admin"
          9 -> "admin"
          _ -> "member",
        "X-Hasura-User-Id" A..= show (fromSqlKey (entityKey eUser)),
        "X-Hasura-Level-Akce" A..= show (maybe 1 permissionLevelAkce permGroup),
        "X-Hasura-Level-Aktuality" A..= show (maybe 1 permissionLevelAktuality permGroup),
        "X-Hasura-Level-Dokumenty" A..= show (maybe 1 permissionLevelDokumenty permGroup),
        "X-Hasura-Level-Galerie" A..= show (maybe 1 permissionLevelGalerie permGroup),
        "X-Hasura-Level-Konzole" A..= show (maybe 1 permissionLevelKonzole permGroup),
        "X-Hasura-Level-Nabidka" A..= show (maybe 1 permissionLevelNabidka permGroup),
        "X-Hasura-Level-Nastenka" A..= show (maybe 1 permissionLevelNastenka permGroup),
        "X-Hasura-Level-Novinky" A..= show (maybe 1 permissionLevelNovinky permGroup),
        "X-Hasura-Level-Pary" A..= show (maybe 1 permissionLevelPary permGroup),
        "X-Hasura-Level-Platby" A..= show (maybe 1 permissionLevelPlatby permGroup),
        "X-Hasura-Level-Permissions" A..= show (maybe 1 permissionLevelPermissions permGroup),
        "X-Hasura-Level-Rozpis" A..= show (maybe 1 permissionLevelRozpis permGroup),
        "X-Hasura-Level-Skupiny" A..= show (maybe 1 permissionLevelSkupiny permGroup),
        "X-Hasura-Level-Users" A..= show (maybe 1 permissionLevelUsers permGroup)
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
