{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Olymp.API
  ( OlympAPI,
    olympAPI,
  )
where

import Control.Effect (Effs)
import Olymp.API.Auth (AuthAPI, authAPI)
import Olymp.API.Event (EventAPI, eventAPI)
import Olymp.API.Photo (PhotoAPI, photoAPI)
import Olymp.API.Schedule (ScheduleAPI, scheduleAPI)
import Olymp.API.User (UserAPI, userAPI)
import Olymp.API.Reservation (ReservationAPI, reservationAPI)
import Olymp.API.Payment (QrPaymentAPI, qrPaymentAPI)
import Olymp.API.WordPress (WordPressAPI, wordPressAPI)
import Olymp.Auth (PhpAuth)
import Olymp.Effect (AppStack)
import Olymp.Tournament.API (tournamentAdminSocket, tournamentSocket)
import Servant
import Servant.API.WebSocket (WebSocket)

type OlympAPI =
  AuthAPI
    :<|> "api" :> UserAPI
    :<|> "api" :> ReservationAPI
    :<|> "api" :> EventAPI
    :<|> "api" :> PhotoAPI
    :<|> "api" :> ScheduleAPI
    :<|> "api" :> QrPaymentAPI
    :<|> "api" :> "tournament" :> "ws" :> WebSocket
    :<|> "api" :> "admin" :> "ws" :> PhpAuth :> WebSocket
    :<|> "wp" :> "v2" :> WordPressAPI

olympAPI :: Effs AppStack m => ServerT OlympAPI m
olympAPI =
  authAPI
    :<|> userAPI
    :<|> reservationAPI
    :<|> eventAPI
    :<|> photoAPI
    :<|> scheduleAPI
    :<|> qrPaymentAPI
    :<|> tournamentSocket
    :<|> tournamentAdminSocket
    :<|> wordPressAPI
