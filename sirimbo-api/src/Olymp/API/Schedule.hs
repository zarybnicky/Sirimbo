{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Olymp.API.Schedule
  ( ScheduleAPI,
    scheduleAPI,
  )
where

import Olymp.Prelude
import Olymp.Schema (Schedule (..), ScheduleId)

type ScheduleAPI =
  PhpAuth :> "schedule" :> Capture "id" ScheduleId :> "toggle-visible" :> Get '[JSON] Bool

scheduleAPI :: Effs '[Error ServerError, Database] m => ServerT ScheduleAPI m
scheduleAPI = toggleVisible

toggleVisible :: Effs '[Error ServerError, Database] m => (SessionId, Entity User) -> ScheduleId -> m Bool
toggleVisible _ k = do
  schedule <- maybe (throw err404) pure =<< query (get k)
  let notVisible = boolToText . not . textToBool $ scheduleVisible schedule
  newSchedule <- query $ updateGet k [ScheduleVisible =. notVisible]
  pure . textToBool $ scheduleVisible newSchedule
