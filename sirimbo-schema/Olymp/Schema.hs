{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Olymp.Schema
  ( PersistEntity(..)
  , EntityField(..)
  , Unique(..)
  , Key(..)
  , module X
  ) where

import Database.Persist

import Olymp.Schema.Announcement as X
import Olymp.Schema.Article as X
import Olymp.Schema.Couple as X
import Olymp.Schema.Document as X
import Olymp.Schema.Event as X
import Olymp.Schema.Migrate as X
import Olymp.Schema.Payment as X
import Olymp.Schema.PaymentGroup as X
import Olymp.Schema.Permission as X
import Olymp.Schema.Photo as X
import Olymp.Schema.Reservation as X
import Olymp.Schema.Runtime as X
import Olymp.Schema.Schedule as X
import Olymp.Schema.User as X
import Olymp.Schema.Video as X
