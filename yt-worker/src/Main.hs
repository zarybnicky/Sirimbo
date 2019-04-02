{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  , printMigrations
  , getChannelUploads
  , redirectPrompt
  , getYTToken
  ) where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (forM)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.Persist.MySQL
import GHC.TypeLits (Symbol)
import Network.Google
import Network.Google.Auth
import Network.Google.YouTube
import System.Exit (exitFailure)
import System.IO (stdout)
import System.Info (os)
import System.Process (rawSystem)

import Schema (migrateAll)

main :: IO ()
main = pure ()

printMigrations :: IO ()
printMigrations = runStdoutLoggingT . withMySQLPool connectInfo 1 . runSqlPool $ printMigration migrateAll
  where
    connectInfo =
      defaultConnectInfo
        { connectHost = "127.0.0.1"
        , connectUser = "olymp"
        , connectPassword = "admin"
        , connectDatabase = "olymp"
        }

-- https://developers.google.com/youtube/v3/docs/channels/list
getChannelUploads :: Text -> IO (Maybe Text, [PlayListItemSnippet])
getChannelUploads chanId = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ youTubeReadOnlyScope)
  runResourceT . runGoogle env $ do
    chan <- send $ channelsList "contentDetails" & cId ?~ chanId
    let uploads = chan ^.. clrItems . each . chaContentDetails . _Just . ccdRelatedPlayLists . _Just . ccdrplUploads . _Just
    plis <- forM uploads $ \(playId :: Text) -> do
      vs <- send (playListItemsList "snippet" & plilPlayListId ?~ playId & plilMaxResults .~ 50)
      pure (vs ^. plilrNextPageToken, vs ^.. plilrItems . each . pliSnippet . _Just)
    pure $ foldr (\(mNext, xs) (mNext', ys) -> (mNext <|> mNext', xs ++ ys)) (Nothing, []) plis

getChannelPlaylists :: Text -> IO [PlayList]
getChannelPlaylists chanId = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ youTubeReadOnlyScope)
  runResourceT . runGoogle env $ do
    lists <- send $ playListsList "snippet" & pllChannelId ?~ chanId
    pure $ lists ^. pllrItems

getYTToken ::
     AllowScopes (s :: [Symbol])
  => OAuthClient
  -> Logger
  -> proxy s
  -> IO (Maybe RefreshToken)
getYTToken c lgr p = do
  mgr <- newManager tlsManagerSettings
  code <- redirectPrompt c p
  auth <- exchange (FromClient c code) lgr mgr
  pure (_tokenRefresh (_token auth))

redirectPrompt :: AllowScopes (s :: [Symbol]) => OAuthClient -> proxy s -> IO (OAuthCode s)
redirectPrompt c p = do
  let url = formURL c p
  T.putStrLn $ "Opening URL " `T.append` url
  _ <- case os of
    "darwin" -> rawSystem "open"     [T.unpack url]
    "linux"  -> rawSystem "xdg-open" [T.unpack url]
    _        -> T.putStrLn "Unsupported OS" >> exitFailure
  T.putStrLn "Please input the authorisation code: "
  OAuthCode <$> T.getLine
