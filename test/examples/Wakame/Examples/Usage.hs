{-# LANGUAGE DuplicateRecordFields #-}
module Wakame.Examples.Usage where

import Prelude

import Data.Kind
import Data.Proxy
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import GHC.TypeLits
import Wakame



newtype UserId = UserId Int
  deriving (Eq, Show, Generic)


data User =
  User
  { id    :: UserId
  , email :: Text
  , password   :: Text
  , created_at :: UTCTime
  , updated_at :: UTCTime
  }
  deriving (Show, Generic)

data UpdatingUser =
  UpdatingUser
  { email    :: Text
  , password :: Text
  }
  deriving (Show, Generic)

updateUser :: UpdatingUser -> User -> User
updateUser updating user = fromRow $ nub $ union (toRow updating) (toRow user)



data UpdatedAt = UpdatedAt { updated_at :: UTCTime }
  deriving (Show, Generic)

touchUser :: UTCTime -> User -> User
touchUser time user = fromRow $ nub $ union (toRow $ UpdatedAt time) (toRow user)



updateAndTouchUser :: UpdatingUser -> UTCTime -> User -> User
updateAndTouchUser updating time user =
  fromRow $ nub $ union (toRow updating) $ union (toRow $ UpdatedAt time) (toRow user)
