{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Lens ((&), (.~), (<&>), (?~))
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Data.Text (Text)
import Database.Persist.MySQL
import Network.Google
import qualified Network.Google.YouTube as YouTube
import System.IO (stdout)

import Schema (migrateAll)


main :: IO ()
main = runStdoutLoggingT $ withMySQLPool connectInfo 1 $ runSqlPool sql
  where
    connectInfo =
      defaultConnectInfo
        { connectHost = "127.0.0.1"
        , connectUser = "olymp"
        , connectPassword = "admin"
        , connectDatabase = "olymp"
        }

sql :: SqlPersistT (LoggingT IO) ()
sql = printMigration migrateAll



-- https://developers.google.com/youtube/v3/docs/channels/list

-- | This gets the Information about an spreadsheet.
-- In order to be able to run these examples you need to
-- create a service acount from google's developers console
-- and copy the dowloaded json file to ~/.config/gcloud/application_default_credentials.json.
--
-- you must also share with your service the spreadsheet that you want to get the info of.
-- In order to do this you must share the sheet with the email address of your service
-- which is in your downloaded service config file.
--
-- after doing above step just pass the sreadsheet id to the function.
exampleGetChannel :: Text -> IO YouTube.ChannelListResponse
exampleGetChannel channelId = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ YouTube.youTubeReadOnlyScope)
  runResourceT . runGoogle env $
    send (YouTube.channelsList "contentDetails" & YouTube.cId ?~ channelId)
