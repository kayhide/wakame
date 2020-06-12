{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Wakame.Examples.Usage where

import Prelude

import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics
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



newtype ID a = ID Int
  deriving (Eq, Show, Generic)

data ModelBase a =
  ModelBase
  { id         :: ID a
  , created_at :: !UTCTime
  , updated_at :: !UTCTime
  }
  deriving (Eq, Show, Generic)


create ::
  forall a b.
  ( IsRow a
  , IsRow b
  , Lacks "id" (Of a)
  , Merge (Of a) (Of (ModelBase b)) (Of b)
  ) => a -> IO b
create x = do
  now <- getCurrentTime
  let y =
        fromRow
        $ merge (toRow x)
        $ toRow $ ModelBase @b (ID 0) now now
  pure y


update ::
  ( IsRow a
  , Merge (Of UpdatedAt) (Of a) (Of a)
  ) => a -> IO a
update x = do
  now <- getCurrentTime
  let y =
        fromRow
        $ merge (toRow $ UpdatedAt now)
        $ toRow x
  pure y
