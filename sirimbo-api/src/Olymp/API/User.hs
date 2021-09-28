{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Olymp.API.User
  ( UserAPI,
    userAPI,
  )
where

import Data.Aeson (ToJSON)
import Data.Csv (Only (..))
import Data.Text (Text)
import Database.Persist.Sql (Entity (..))
import Olymp.Auth (PhpAuth)
import Olymp.Effect.Log (WithLog, logInfo)
import Olymp.Schema (SessionId, User (..))
import Servant
import Servant.CSV.Cassava (CSV', DefaultOpts, HasHeader (NoHeader))

deriving newtype instance ToJSON a => ToJSON (Only a)

type UserAPI =
  PhpAuth :> "export-emails" :> Get '[JSON, CSV' 'NoHeader DefaultOpts] [Only Text]
    :<|> PhpAuth :> "whoami" :> Get '[PlainText, JSON] Text

userAPI :: (WithLog m) => ServerT UserAPI m
userAPI = exportEmails :<|> whoAmI

exportEmails :: WithLog m => (SessionId, Entity User) -> m [Only Text]
exportEmails _ = pure mempty

whoAmI :: WithLog m => (SessionId, Entity User) -> m Text
whoAmI (_, eu) = do
  let u = entityVal eu
  logInfo (userName u)
  pure (userName u <> " " <> userSurname u)
