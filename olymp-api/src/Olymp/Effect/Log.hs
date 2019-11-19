{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Effect.Log
  ( Log(..)
  , WithLog
  , logDebug
  , logInfo
  , logWarning
  , logError
  , logException
  , runLog
  , runLogToStdout
  ) where

import Colog.Core (Severity(..))
import Colog.Core.Action (LogAction (..))
import Colog.Actions (simpleMessageAction)
import Colog.Message (Msg(..))
import Control.Exception (Exception, displayException)
import Data.Kind (Type)
import Data.Text (Text, pack)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Polysemy (Embed, Member, Sem, interpret, makeSem)
import Prelude hiding (log)

data Log (m :: Type -> Type) (a :: Type) where
    Log :: Msg Severity -> Log m ()

$(makeSem ''Log)

type WithLog r = (Member Log r, HasCallStack)

runLog
    :: forall r a .
       LogAction (Sem r) (Msg Severity)
    -> Sem (Log ': r) a
    -> Sem r a
runLog (LogAction action) = interpret $ \case
    Log msg -> action msg
{-# INLINE runLog #-}

runLogToStdout :: Member (Embed IO) r => Sem (Log ': r) a -> Sem r a
runLogToStdout = runLog simpleMessageAction

log_ :: WithLog r => Severity -> Text -> Sem r ()
log_ sev msg = withFrozenCallStack (log $ Msg sev callStack msg)

logDebug :: WithLog r => Text -> Sem r ()
logDebug = withFrozenCallStack (log_ Debug)

logInfo :: WithLog r => Text -> Sem r ()
logInfo = withFrozenCallStack (log_ Info)

logWarning :: WithLog r => Text -> Sem r ()
logWarning = withFrozenCallStack (log_ Warning)

logError :: WithLog r => Text -> Sem r ()
logError = withFrozenCallStack (log_ Error)

logException :: (WithLog r, Exception e) => e -> Sem r ()
logException = withFrozenCallStack (log_ Error . pack . displayException)
