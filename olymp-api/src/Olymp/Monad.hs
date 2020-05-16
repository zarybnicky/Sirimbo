{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Olymp.Monad
  ( AppStack
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
import Polysemy (Embed, Sem, runM)
import Polysemy.AtomicState (AtomicState, runAtomicStateIORef)
import Polysemy.Error (Error, runError)
import Polysemy.Resource (Resource, resourceToIO)
import Servant (Handler(..), ServerError)

type AppStack
   = '[ UserEff
      , SessionEff
      , AppError
      , Error ServerError
      , AtomicState (Map Int Connection)
      , AtomicState (Tournament NodeId)
      , Database SqlBackend
      , Resource
      , Log
      , Embed IO
      ]

interpretServer ::
     IORef (Tournament NodeId)
  -> IORef (Map Int Connection)
  -> Pool SqlBackend
  -> Sem AppStack a
  -> Handler a
interpretServer ref ref' pool =
  Handler . ExceptT . runM .
  runLogToStdout .
  resourceToIO .
  runDatabasePool pool .
  runAtomicStateIORef ref .
  runAtomicStateIORef ref' .
  runError @ServerError .
  runAppErrorToError .
  runSessionEffPersistent .
  runUserEffPersistent
