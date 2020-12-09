{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Olymp.Effect.Google
  ( GoogleEff(..)
  , sendGoogle
  , sendG
  , googleToResourceTIO
  ) where

import Control.Effect (Eff, Embed, SimpleInterpreterFor, send, interpretSimple)
import Control.Effect.Embed (embed)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (ResourceT, liftResourceT)
import Network.Google (AllowScopes, Google(..), GoogleRequest, HasScope, Rs, send)
import Network.Google.Env (Env)

data GoogleEff s m a where
  SendGoogle :: Google s a -> GoogleEff s m a

sendGoogle, sendG
  :: forall s m a. (AllowScopes s, Eff (GoogleEff s) m, HasScope s a, GoogleRequest a)
  => a
  -> m (Rs a)
sendGoogle = Control.Effect.send . SendGoogle . Network.Google.send @s
sendG = Control.Effect.send . SendGoogle . Network.Google.send @s

googleToResourceTIO
  :: forall s m. Eff (Embed (ResourceT IO)) m
  => Env s
  -> SimpleInterpreterFor (GoogleEff s) m
googleToResourceTIO env = interpretSimple $ \case
  SendGoogle f -> embed @(ResourceT IO) $ liftResourceT $ runReaderT (unGoogle f) env
