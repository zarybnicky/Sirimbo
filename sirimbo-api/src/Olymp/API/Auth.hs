{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Olymp.API.Auth
  ( AuthAPI,
    authAPI,
  )
where

import Control.Effect (Eff)
import Data.Time (Day (ModifiedJulianDay), UTCTime (..))
import Olymp.Effect.Session (SessionEff, deleteSession)
import Servant
import Web.Cookie (SetCookie (..), defaultSetCookie)
import qualified Data.Aeson as A
import Olymp.Prelude
import Olymp.Schema (User(userPermission), Permission (..))
import Database.Persist.Sql (fromSqlKey)

type AuthAPI =
  PhpMaybeAuth :> "api" :> "graphql-auth" :> Get '[JSON] A.Value :<|>
  PhpAuth :> "logout" :> Verb 'GET 303 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)

authAPI :: Effs '[Database, SessionEff] m => ServerT AuthAPI m
authAPI = graphqlAuth :<|> logout

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
        "X-Hasura-User-Id" A..= fromSqlKey (entityKey eUser),
        "X-Hasura-Level-Akce" A..= maybe 1 permissionLevelAkce permGroup,
        "X-Hasura-Level-Aktuality" A..= maybe 1 permissionLevelAktuality permGroup,
        "X-Hasura-Level-Dokumenty" A..= maybe 1 permissionLevelDokumenty permGroup,
        "X-Hasura-Level-Galerie" A..= maybe 1 permissionLevelGalerie permGroup,
        "X-Hasura-Level-Konzole" A..= maybe 1 permissionLevelKonzole permGroup,
        "X-Hasura-Level-Nabidka" A..= maybe 1 permissionLevelNabidka permGroup,
        "X-Hasura-Level-Nastenka" A..= maybe 1 permissionLevelNastenka permGroup,
        "X-Hasura-Level-Novinky" A..= maybe 1 permissionLevelNovinky permGroup,
        "X-Hasura-Level-Pary" A..= maybe 1 permissionLevelPary permGroup,
        "X-Hasura-Level-Platby" A..= maybe 1 permissionLevelPlatby permGroup,
        "X-Hasura-Level-Permissions" A..= maybe 1 permissionLevelPermissions permGroup,
        "X-Hasura-Level-Rozpis" A..= maybe 1 permissionLevelRozpis permGroup,
        "X-Hasura-Level-Skupiny" A..= maybe 1 permissionLevelSkupiny permGroup,
        "X-Hasura-Level-Users" A..= maybe 1 permissionLevelUsers permGroup
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
