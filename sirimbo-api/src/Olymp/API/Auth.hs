{-# LANGUAGE DataKinds #-}
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
import Database.Persist.Sql (Entity)
import Olymp.Auth (PhpAuth)
import Olymp.Effect.Session (SessionEff, deleteSession)
import Olymp.Schema (SessionId, User (..))
import Servant
import Web.Cookie (SetCookie (..), defaultSetCookie)

type AuthAPI =
  PhpAuth :> "logout"
    :> Verb 'GET 303 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Location" String] NoContent)

authAPI :: Eff SessionEff m => ServerT AuthAPI m
authAPI = logout

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
