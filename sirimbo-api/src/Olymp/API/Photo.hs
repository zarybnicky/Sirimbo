{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Olymp.API.Photo
  ( PhotoAPI,
    photoAPI,
  )
where

import Olymp.Prelude
import Olymp.Schema (PhotoDirectory (..), PhotoDirectoryId)

type PhotoAPI =
  PhpAuth :> "photo" :> "directory" :> Capture "id" PhotoDirectoryId
    :> "toggle-visible"
    :> OpId "toggleVisiblePhotoDirectory"
    :> Get '[JSON] Bool
  :<|> "photo"
    :> "directory"
    :> OpId "getPhotoDirectories"
    :> Get '[JSON] [PhotoDirectory]

photoAPI :: Effs '[Error ServerError, Database] m => ServerT PhotoAPI m
photoAPI = toggleVisible :<|> getAll

toggleVisible :: Effs '[Error ServerError, Database] m => (SessionId, Entity User) -> PhotoDirectoryId -> m Bool
toggleVisible _ k = do
  photo <- maybe (throw err404) pure =<< query (get k)
  let notVisible = not $ photoDirectoryHidden photo
  newPhoto <- query $ updateGet k [PhotoDirectoryHidden =. notVisible]
  pure $ photoDirectoryHidden newPhoto

getAll :: Effs '[Database] m => m [PhotoDirectory]
getAll = fmap entityVal <$> query (selectList [] [])
