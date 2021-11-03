{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Olymp.Schema.Runtime
  ( Key(..)
  , EntityField(..)
  , ParameterId
  , Parameter(..)
  , SessionId
  , Session(..)
  , UpdateLogId
  , UpdateLog(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Key, EntityField)
import Olymp.Schema.Orphans ()
import Olymp.Schema.User (UserId)
import Olymp.Schema.Utils (mkPersist', persistSchema)

mkPersist' $(persistSchema "Runtime.model")
