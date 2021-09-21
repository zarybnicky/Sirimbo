{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Olymp.Schema.Video
  ( Key(..)
  , EntityField(..)
  , VideoId
  , Video(..)
  , VideoListId
  , VideoList(..)
  , VideoSourceId
  , VideoSource(..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Key, EntityField)
import Olymp.Schema.Utils (mkPersist', persistSchema)

mkPersist' $(persistSchema "Video.model")
