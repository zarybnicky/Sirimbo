{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE DataKinds #-}
module Olymp.Effect
  ( AppStack,
    AppC,
    interpretServer
  )
where

import Control.Effect (CompositionC, InterpretSimpleC, runComposition, runM)
import Control.Effect.AtomicState (AtomicState, runAtomicStateIORefSimple)
import Control.Effect.Bracket (Bracket, BracketToIOC, bracketToIO)
import Control.Effect.Embed (Embed, RunMC)
import Control.Effect.Error (Error, ErrorC, runError)
import Data.Map (Map)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import Network.WebSockets (Connection)
import Olymp.Effect.Database (Database, runDatabasePool)
import Olymp.Effect.Error (AppError, runAppErrorToError)
import Olymp.Effect.Log (Log, runLogToStdout)
import Olymp.Effect.Session (SessionEff, runSessionEffPersistent)
import Olymp.Effect.User (UserEff, runUserEffPersistent)
import Olymp.Tournament.Base (NodeId, Tournament)
import Servant.Server.Internal.ServerError (ServerError)
import Data.IORef (IORef)

type AppStack =
  '[ UserEff,
     SessionEff,
     AppError,
     Error ServerError,
     AtomicState (Map Int Connection),
     AtomicState (Tournament NodeId),
     Database,
     Bracket,
     Log,
     Embed IO
   ]

type AppC =
  CompositionC
    '[ InterpretSimpleC UserEff,
       InterpretSimpleC SessionEff,
       InterpretSimpleC AppError,
       ErrorC ServerError,
       InterpretSimpleC (AtomicState (Map Int Connection)),
       InterpretSimpleC (AtomicState (Tournament NodeId)),
       InterpretSimpleC Database,
       InterpretSimpleC Log,
       BracketToIOC
     ]

interpretServer ::
  IORef (Tournament NodeId) ->
  IORef (Map Int Connection) ->
  Pool SqlBackend ->
  AppC (RunMC IO) a ->
  IO (Either ServerError a)
interpretServer ref ref' pool f =
  runM $
    bracketToIO $
      runLogToStdout $
        runDatabasePool pool $
          runAtomicStateIORefSimple ref $
            runAtomicStateIORefSimple ref' $
              runError @ServerError $
                runAppErrorToError $
                  runSessionEffPersistent $
                    runUserEffPersistent $
                      runComposition f
