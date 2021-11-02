{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.API
  ( OlympAPI,
    olympAPI,
  )
where

import Control.Effect (Effs)
import Olymp.API.Auth (AuthAPI, authAPI)
import Olymp.API.Photo (PhotoAPI, photoAPI)
import Olymp.API.User (UserAPI, userAPI)
import Olymp.API.Payment (PaymentAPI, paymentAPI)
import Olymp.Auth (PhpAuth)
import Olymp.Effect (AppStack)
import Olymp.Tournament.API (tournamentAdminSocket, tournamentSocket)
import Servant
import Servant.API.WebSocket (WebSocket)

type OlympAPI =
  AuthAPI
    :<|> "api" :> UserAPI
    :<|> "api" :> PhotoAPI
    :<|> "api" :> PaymentAPI
    :<|> "api" :> "tournament" :> "ws" :> WebSocket
    :<|> "api" :> "admin" :> "ws" :> PhpAuth :> WebSocket

olympAPI :: Effs AppStack m => ServerT OlympAPI m
olympAPI =
  authAPI
    :<|> userAPI
    :<|> photoAPI
    :<|> paymentAPI
    :<|> tournamentSocket
    :<|> tournamentAdminSocket
