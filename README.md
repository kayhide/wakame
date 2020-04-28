# Wakame

`wakame` is a library to manipulate records generically.


# Usage

Here is a data type.

```haskell
data User =
  User
  { id :: UserId
  , email :: Text
  , passowrd :: Text
  , created_at :: UTCTime
  , updated_at :: UTCTime
  }
  deriving Generic
```

When letting end users to update its fields, you may want them to touch only the subset of it.

To do so, define the exposing fields as:

```haskell
data UpdatingUser =
  UpdatingUser
  { email :: Text
  , password :: Text
  }
  deriving Generic
```

And update the user record with an updating data:


```haskell
updateUser :: UpdatingUser -> User -> User 
updateUser updating user = fromRec $ nub $ union (toRec updating) (toRec user)
```

Accordingly, to update the `updated_at` field:


```haskell
data UpdatedAt = UpdatedAt { updated_at :: UTCTime } deriving Generic

touchUser :: UTCTime -> User -> User
touchUser time user = fromRec $ nub $ union (toRec $ UpdatedAt time) (toRec user)
```

To do them at once:


```haskell
updateAndTouchUser :: UpdatingUser -> UTCTime -> User -> User
updateAndTouchUser updating time user =
  fromRec $ nub $ union (toRec $ updating) $ union (toRec $ UpdatedAt time) (toRec user)
```

Note that you can defer to `nub` until fully `union`ed.
