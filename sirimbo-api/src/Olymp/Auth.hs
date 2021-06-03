{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Olymp.Auth
  ( PhpAuth
  , PhpAuthHandler
  , SessionId
  , Entity(..)
  , User(..)
  , phpAuthHandler
  ) where

import Control.Effect (Effs)
import Data.Aeson (FromJSON(..), decodeStrict', withObject, (.:))
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sql (Entity(..), toSqlKey)
import Network.Wai (Request, requestHeaders)
import Olymp.Effect.Error (AppError, AuthError(..), throwAuth)
import Olymp.Effect.Session (SessionEff, getSessionById, deleteSession)
import Olymp.Effect.User (UserEff, getUserById)
import Olymp.Schema (Key(..), Session(..), SessionId, User(..), UserId)
import Servant (AuthProtect, Handler)
import Servant.Server.Experimental.Auth
  (AuthHandler, AuthServerData, mkAuthHandler)
import Web.Cookie (parseCookies)

type PhpAuth = AuthProtect "php-session"
type PhpAuthHandler = AuthHandler Request (SessionId, Entity User)

type instance AuthServerData PhpAuth = (SessionId, Entity User)

newtype SessionUserId = SessionUserId
  { unSessionUserId :: UserId
  } deriving (Show)

instance FromJSON SessionUserId where
  parseJSON = withObject "Session" $ \o ->
    SessionUserId . toSqlKey <$> o .: "id"

phpAuthHandler ::
     forall m. Effs '[ AppError, UserEff, SessionEff] m
  => (forall a. m a -> Handler a)
  -> PhpAuthHandler
phpAuthHandler runner = mkAuthHandler $ runner . getUserByCookie

getUserByCookie :: forall m. Effs '[ AppError, UserEff, SessionEff] m => Request -> m (SessionId, Entity User)
getUserByCookie req = do
  cookie <- maybeErr . lookup "cookie" $ requestHeaders req
  sid' <- maybeErr . lookup "PHPSESSID" $ parseCookies cookie
  let sid = SessionKey (decodeUtf8 sid')
  sess <- maybeErr =<< getSessionById sid
  uid <- maybeDelete sid . decodeStrict' $ sessionData sess
  maybeDelete sid . fmap (sid, ) =<< getUserById (unSessionUserId uid)
  where
    maybeErr :: Maybe a -> m a
    maybeErr = maybe (throwAuth ErrNotLoggedIn) pure
    maybeDelete :: SessionId -> Maybe a -> m a
    maybeDelete sid = flip maybe pure $ do
      deleteSession sid
      throwAuth ErrNotLoggedIn
