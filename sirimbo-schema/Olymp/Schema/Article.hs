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

module Olymp.Schema.Article
  ( Key(..)
  , EntityField(..)
  , ArticleId
  , Article(..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Key, EntityField)
import Olymp.Schema.Photo (PhotoId)
import Olymp.Schema.User (UserId)
import Olymp.Schema.Utils (mkPersist', persistSchema)

mkPersist' $(persistSchema "Article.model")
