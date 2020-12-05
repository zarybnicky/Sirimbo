{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.Effect.Log
  ( Log(..)
  , WithLog
  , logDebug
  , logInfo
  , logWarning
  , logError
  , logException
  -- , runLog
  , runLogToStdout
  ) where

import Colog.Core (Severity(..))
import Colog.Core.Action (LogAction (..))
import Colog.Actions (simpleMessageAction)
import Colog.Message (Msg(..))
import Control.Effect (Eff, Effs, Embed, SimpleInterpreterFor, interpretSimple, send)
import Control.Effect.Embed (embed)
import Control.Exception (Exception, displayException)
import Data.Text (Text, pack)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Prelude hiding (log)

data Log m a where
    Log :: Msg Severity -> Log m ()

log :: Eff Log m => Msg Severity -> m ()
log = send . Log

type WithLog m = (Eff Log m, HasCallStack)

-- runLog :: forall m. Eff Log m => LogAction m (Msg Severity) -> SimpleInterpreterFor Log m
-- runLog action = interpretSimple $ \case
--   Log msg -> unLogAction action msg

runLogToStdout :: Effs '[Embed IO] m => SimpleInterpreterFor Log m
runLogToStdout = interpretSimple $ \case
  Log msg -> embed (unLogAction simpleMessageAction msg :: IO ())

log_ :: WithLog m => Severity -> Text -> m ()
log_ sev msg = withFrozenCallStack (log $ Msg sev callStack msg)

logDebug :: WithLog m => Text -> m ()
logDebug = withFrozenCallStack (log_ Debug)

logInfo :: WithLog m => Text -> m ()
logInfo = withFrozenCallStack (log_ Info)

logWarning :: WithLog m => Text -> m ()
logWarning = withFrozenCallStack (log_ Warning)

logError :: WithLog m => Text -> m ()
logError = withFrozenCallStack (log_ Error)

logException :: (WithLog m, Exception e) => e -> m ()
logException = withFrozenCallStack (log_ Error . pack . displayException)
