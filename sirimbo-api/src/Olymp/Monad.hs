{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Olymp.Monad
  ( AppC
  , AppStack
  , interpretServer
  ) where

import Control.Monad.Except (ExceptT(..))
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Pool (Pool)
import Database.Persist.MySQL (SqlBackend)
import Network.WebSockets (Connection)
import Olymp.Effect.Database (Database, runDatabasePool)
import Olymp.Effect.Error (AppError, runAppErrorToError)
import Olymp.Effect.Log (Log, runLogToStdout)
import Olymp.Effect.User (UserEff, runUserEffPersistent)
import Olymp.Effect.Session (SessionEff, runSessionEffPersistent)
import Olymp.Tournament (Tournament, NodeId)
import Control.Effect (InterpretSimpleC, runM)
import Control.Effect.AtomicState (AtomicState, runAtomicStateIORefSimple)
import Control.Effect.Bracket (BracketToIOC, Bracket, bracketToIO)
import Control.Effect.Embed (Embed)
import Control.Effect.Error (ErrorC, Error, runError)
import Servant (Handler(..), ServerError)
import Control.Effect.Embed (RunMC)

type AppStack
   = '[ UserEff
      , SessionEff
      , AppError
      , Error ServerError
      , AtomicState (Map Int Connection)
      , AtomicState (Tournament NodeId)
      , Database SqlBackend
      , Bracket
      , Log
      , Embed IO
      ]

type AppC = InterpretSimpleC UserEff
  (InterpretSimpleC SessionEff
   (InterpretSimpleC AppError
    (ErrorC ServerError
     (InterpretSimpleC (AtomicState (Map Int Connection))
      (InterpretSimpleC (AtomicState (Tournament NodeId))
       (InterpretSimpleC (Database SqlBackend)
        (BracketToIOC
         (InterpretSimpleC Log
          (RunMC IO)))))))))

interpretServer ::
  IORef (Tournament NodeId)
  -> IORef (Map Int Connection)
  -> Pool SqlBackend
  -> AppC a
  -> Handler a
interpretServer ref ref' pool f =
  Handler $ ExceptT $
  runM @IO $
  runLogToStdout $
  bracketToIO $
  runDatabasePool pool $
  runAtomicStateIORefSimple ref $
  runAtomicStateIORefSimple ref' $
  runError @ServerError $
  runAppErrorToError $
  runSessionEffPersistent @SqlBackend $
  runUserEffPersistent @SqlBackend f
