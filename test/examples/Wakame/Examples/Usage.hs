{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Wakame.Examples.Usage where

import Prelude

import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics
import Wakame


newtype ID a = ID Int
  deriving (Eq, Show, Generic)

data User =
  User
  { id         :: ID User
  , email      :: Text
  , username   :: Text
  , created_at :: UTCTime
  , updated_at :: UTCTime
  }
  deriving (Show, Generic)

data UpdatingUser =
  UpdatingUser
  { email    :: Text
  , username :: Text
  }
  deriving (Show, Generic)

updateUser :: UpdatingUser -> User -> User
updateUser updating user = fromRow $ nub $ union (toRow updating) (toRow user)

touchUser :: UTCTime -> User -> User
touchUser time user = fromRow $ nub $ union (toRow $ keyed @"updated_at" time) (toRow user)

updateAndTouchUser :: UpdatingUser -> UTCTime -> User -> User
updateAndTouchUser updating time user =
  fromRow $ nub $ union (toRow updating) $ union (toRow $ keyed @"updated_at" time) (toRow user)


data ModelBase a =
  ModelBase
  { id         :: ID a
  , created_at :: UTCTime
  , updated_at :: UTCTime
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
  id' <- pure $ ID @b 42 -- shall be `getNextID` or something in practice.
  let y =
        fromRow
        $ merge (toRow x)
        $ toRow $ ModelBase @b id' now now
  pure y

type OfUpdatedAt = '[ '("updated_at", UTCTime) ]
update ::
  ( IsRow a
  , IsRow b
  , Union (Of a) (Of b) ab
  , Merge OfUpdatedAt ab (Of b)
  ) => a -> b -> IO b
update updating x = do
  now <- getCurrentTime
  let y =
        fromRow
        $ merge (toRow $ keyed @"updated_at" now)
        $ union (toRow updating)
        $ toRow x
  pure y
