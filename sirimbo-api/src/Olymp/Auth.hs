{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Auth
  ( PhpAuth
  , PhpAuthHandler
  , phpAuthHandler
  ) where

import Data.Aeson (FromJSON(..), decodeStrict', withObject, (.:))
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sql (toSqlKey)
import Network.Wai (Request, requestHeaders)
import Olymp.Effect.Error (AppError, AuthError(..), throwAuth)
import Olymp.Effect.Session (SessionEff, getSessionById, deleteSession)
import Olymp.Effect.User (UserEff, getUserById)
import Olymp.Schema (Key(..), Session(..), SessionId, User, UserId)
import Polysemy (Members, Sem)
import Servant (AuthProtect, Handler)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Web.Cookie (parseCookies)

type PhpAuth = AuthProtect "php-session"
type PhpAuthHandler = AuthHandler Request User

type instance AuthServerData PhpAuth = User

newtype SessionUserId = SessionUserId
  { unSessionUserId :: UserId
  } deriving (Show)
instance FromJSON SessionUserId where
  parseJSON = withObject "Session" $ \o ->
    SessionUserId . toSqlKey <$> o .: "id"

phpAuthHandler ::
     forall r. Members '[ AppError, UserEff, SessionEff] r
  => (forall a. Sem r a -> Handler a)
  -> PhpAuthHandler
phpAuthHandler runner = mkAuthHandler $ \req -> runner $ do
  cookie <- maybeErr . lookup "cookie" $ requestHeaders req
  sid' <- maybeErr . lookup "PHPSESSID" $ parseCookies cookie
  let sid = SessionKey (decodeUtf8 sid')
  sess <- maybeErr =<< getSessionById sid
  uid <- maybeDelete sid . decodeStrict' $ sessionData sess
  maybeDelete sid =<< getUserById (unSessionUserId uid)
  where
    maybeErr :: Maybe a -> Sem r a
    maybeErr = maybe (throwAuth ErrNotLoggedIn) pure
    maybeDelete :: SessionId -> Maybe a -> Sem r a
    maybeDelete sid = flip maybe pure $ do
      deleteSession sid
      throwAuth ErrNotLoggedIn
