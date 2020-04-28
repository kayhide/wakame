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
import Wakame.Utils (Append)



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
updateUser updating user = fromRec $ nub $ union (toRec updating) (toRec user)



data UpdatedAt = UpdatedAt { updated_at :: UTCTime }
  deriving (Show, Generic)

touchUser :: UTCTime -> User -> User
touchUser time user = fromRec $ nub $ union (toRec $ UpdatedAt time) (toRec user)



updateAndTouchUser :: UpdatingUser -> UTCTime -> User -> User
updateAndTouchUser updating time user =
  fromRec $ nub $ union (toRec updating) $ union (toRec $ UpdatedAt time) (toRec user)
