{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Olymp
  ( runServer
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Effect (Effs, Embed, embed, runM)
import Control.Effect.AtomicState (runAtomicStateIORefSimple, AtomicState, atomicGet)
import Control.Effect.Bracket (bracketToIO)
import Control.Effect.Embed (embedToMonadIO)
import Control.Lens
import Control.Monad.Except (ExceptT(..), void, forever)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (encode, eitherDecode')
import qualified Data.ByteString.Lazy.Char8 as BCL
import Data.IORef (newIORef)
import qualified Data.Map as M
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import Database.Persist.Sql (SqlBackend, get, repsert, printMigration, runMigration)
import Database.Persist.MySQL (ConnectInfo(..), createMySQLPool, defaultConnectInfo)
import Network.Google (LogLevel(Info), envScopes, newEnvWith, tlsManagerSettings, newManager, newLogger)
import Network.Google.Auth (getApplicationDefault)
import Network.Google.YouTube (youTubeReadOnlyScope)
import Network.Wai.Handler.Warp (run)
-- import Network.Wai.Middleware.Cors
import Olymp.Cli (Command(..), Config(..), parseCli)
import Olymp.Effect.Database (Database, query, runDatabasePool)
import Olymp.Effect.Google (googleToResourceTIO)
import Olymp.Schema (migrateAll, Key(ParameterKey), Parameter(..))
import Olymp.Server (olympServer, interpretServer)
import Olymp.Tournament.API (initialTournament)
import Olymp.Tournament.Base (Tournament, NodeId, withTournament, propagateWinners)
import Olymp.YouTube.Worker (youtubeWorker)
import Servant (Handler(..))
-- import Servant.Multipart (MultipartData, MultipartForm, Tmp)
import System.IO (stdout)

runServer :: IO ()
runServer = do
  (config, command) <- parseCli
  case command of
    Server port -> do
      pool <- makePool config
      t <- withResource pool (runReaderT (get $ ParameterKey "tournament")) >>= \case
        Nothing -> pure initialTournament
        Just (Parameter tt) -> case eitherDecode' (BCL.pack $ T.unpack tt) of
          Left err -> putStrLn err >> pure initialTournament
          Right tt' -> pure tt'

      ref <- newIORef (withTournament propagateWinners t)
      ref' <- newIORef M.empty

      _ <- forkIO . forever . runM . runDatabasePool pool . runAtomicStateIORefSimple ref $
        saveTournamentState

      putStrLn ("Starting server on port " <> show port)
      run port $ olympServer (Handler . ExceptT . interpretServer ref ref' pool)

    CheckYouTube -> do
      lgr <- newLogger Network.Google.Info stdout
      mgr <- newManager tlsManagerSettings
      cred <- getApplicationDefault mgr
      env <- newEnvWith cred lgr mgr <&> (envScopes .~ youTubeReadOnlyScope)
      pool <- makePool config

      runResourceT . runM . embedToMonadIO . bracketToIO . runDatabasePool pool . googleToResourceTIO env $
        youtubeWorker

    Migrate realExecute -> do
      pool <- makePool config
      runM . runDatabasePool pool . query $
        if realExecute then runMigration migrateAll else printMigration migrateAll

makePool :: Config -> IO (Pool SqlBackend)
makePool config = runStdoutLoggingT $ createMySQLPool connectInfo 5
  where
    connectInfo =
      maybe id (\p x -> x { connectPassword = p }) (dbPassword config) $
      defaultConnectInfo
        { connectHost = dbHost config
        , connectUser = dbUser config
        , connectDatabase = dbDatabase config
        }

saveTournamentState :: Effs '[AtomicState (Tournament NodeId), Database, Embed IO] m => m ()
saveTournamentState = do
  embed $ threadDelay 100000000
  state <- T.pack . BCL.unpack . encode <$> atomicGet @(Tournament NodeId)
  void . query $ repsert (ParameterKey "tournament") (Parameter state)