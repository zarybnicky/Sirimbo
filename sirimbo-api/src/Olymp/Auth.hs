{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Olymp.Auth
  ( PhpAuth,
    PhpAuthHandler,
    PhpMaybeAuth,
    PhpMaybeAuthHandler,
    SessionId,
    Entity (..),
    User (..),
    phpAuthHandler,
    phpMaybeAuthHandler,
  )
where

import Control.Effect (Effs)
import Data.Aeson (FromJSON (..), Result (..), fromJSON, withObject, (.:))
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Sql (Entity (..), toSqlKey)
import Network.Wai (Request, requestHeaders)
import Olymp.Effect.Error (AppError, AuthError (..), throwAuth)
import Olymp.Effect.Session (SessionEff, deleteSession, getSessionById)
import Olymp.Effect.User (UserEff, getUserById)
import Olymp.Schema (Key (..), Session (..), SessionId, User (..), UserId)
import Servant (AuthProtect, Handler)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Web.Cookie (parseCookies)

type PhpAuth = AuthProtect "php-session"

type PhpAuthHandler = AuthHandler Request (SessionId, Entity User)

type PhpMaybeAuth = AuthProtect "php-session-maybe"

type PhpMaybeAuthHandler = AuthHandler Request (Maybe (SessionId, Entity User))

type instance AuthServerData PhpAuth = (SessionId, Entity User)

type instance AuthServerData PhpMaybeAuth = Maybe (SessionId, Entity User)

newtype SessionUserId = SessionUserId
  { unSessionUserId :: UserId
  }
  deriving (Show)

instance FromJSON SessionUserId where
  parseJSON = withObject "Session" $ \o ->
    SessionUserId . toSqlKey <$> o .: "id"

phpAuthHandler ::
  forall m.
  Effs '[AppError, UserEff, SessionEff] m =>
  (forall a. m a -> Handler a) ->
  PhpAuthHandler
phpAuthHandler runner = mkAuthHandler $ runner . getUserByCookie

getUserByCookie :: forall m. Effs '[AppError, UserEff, SessionEff] m => Request -> m (SessionId, Entity User)
getUserByCookie req = do
  cookie <- maybeErr . lookup "cookie" $ requestHeaders req
  sid <- fmap (SessionKey . BS.unpack) . maybeErr . lookup "PHPSESSID" $ parseCookies cookie
  sess <- maybeErr =<< getSessionById sid
  uid <- maybeDelete sid . resultToMaybe . fromJSON $ sessionContents sess
  maybeDelete sid . fmap (sid,) =<< getUserById (unSessionUserId uid)
  where
    maybeErr :: Maybe a -> m a
    maybeErr = maybe (throwAuth ErrNotLoggedIn) pure
    resultToMaybe :: Result a -> Maybe a
    resultToMaybe = \case
      Error _ -> Nothing
      Success a -> Just a
    maybeDelete :: SessionId -> Maybe a -> m a
    maybeDelete sid = flip maybe pure $ do
      deleteSession sid
      throwAuth ErrNotLoggedIn

phpMaybeAuthHandler ::
  forall m.
  Effs '[AppError, UserEff, SessionEff] m =>
  (forall a. m a -> Handler a) ->
  PhpMaybeAuthHandler
phpMaybeAuthHandler runner = mkAuthHandler $ runner . getMaybeUserByCookie

getMaybeUserByCookie :: forall m. Effs '[UserEff, SessionEff] m => Request -> m (Maybe (SessionId, Entity User))
getMaybeUserByCookie req = do
  case lookup "cookie" (requestHeaders req) of
    Nothing -> pure Nothing
    Just cookie -> do
      case fmap (SessionKey . BS.unpack) (lookup "PHPSESSID" (parseCookies cookie)) of
        Nothing -> pure Nothing
        Just sid -> do
          sess <- getSessionById sid
          case fromJSON . sessionContents <$> sess of
            Nothing -> pure Nothing
            Just (Error _) -> pure Nothing
            Just (Success uid) -> do
              getUserById (unSessionUserId uid) >>= \case
                Nothing -> pure Nothing
                Just eUser -> pure $ Just (sid, eUser)
