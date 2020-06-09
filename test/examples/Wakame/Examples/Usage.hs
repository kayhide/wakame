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
  forall a b attrs.
  ( IsRow a
  , IsRow b
  , Lacks "id" (Of a)
  , Union (Of a) (Of (ModelBase b)) attrs
  , Nub attrs (Of b)
  ) => a -> IO b
create x = do
  now <- getCurrentTime
  let y =
        fromRow $ nub
        $ union (toRow x)
        $ toRow $ ModelBase (ID @b 0) now now
  pure y


update ::
  ( IsRow a
  , Union (Of UpdatedAt) (Of a) attrs
  , Nub attrs (Of a)
  ) => a -> IO a
update x = do
  now <- getCurrentTime
  let y =
        fromRow $ nub
        $ union (toRow $ UpdatedAt now)
        $ toRow x
  pure y
