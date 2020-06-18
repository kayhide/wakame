# Wakame

hackage? comming soon

`wakame` is a Haskell library to manipulate record fields in a row-polymorphic way.

## Overview

Here is a quick overview of what `wakame` provides.

Imagine a data type of:

```haskell
data User =
  User
  { id :: ID User
  , email :: Text
  , username :: Text
  , created_at :: UTCTime
  , updated_at :: UTCTime
  }
  deriving Generic
```

To update a subset of the `User` record's fields, first define a data type containing the fields you want to update:

```haskell
data UpdatingUser =
  UpdatingUser
  { email :: Text
  , username :: Text
  }
  deriving Generic
```

Then, write a function for doing the update:


```haskell
updateUser :: UpdatingUser -> User -> User
updateUser updating user = fromRec $ nub $ union (toRec updating) (toRec user)
```

Here is a working example of using this function:

```haskell
> user
User {id = ID 42, email = "peter@amazing.com", username = "Peter Parker", created_at = 2020-06-16 11:22:11.991147596 UTC, updated_at = 2020-06-16 11:22:11.991147596 UTC}
> updating
UpdatingUser {email = "spider@man.com", username = "Spider Man"}
> updateUser updating user
User {id = ID 42, email = "spider@man.com", username = "Spider Man", created_at = 2020-06-16 11:22:11.991147596 UTC, updated_at = 2020-06-16 11:22:11.991147596 UTC}
```

Updating the `updated_at` field in `User` can be done in the same manner.  But
this time, let's do it without defining a separate record type:

```haskell
touchUser :: UTCTime -> User -> User
touchUser time user = fromRec $ nub $ union (toRec $ keyed @"updated_at" time) (toRec user)
```

`toRec $ keyed @"update_at" time` creates a `Row` object which has only one field:

```haskell
{ updated_at :: UTCTime }
```

And updating the user and the `updated_at` field can be done easily within the
same function:

```haskell
updateAndTouchUser :: UpdatingUser -> UTCTime -> User -> User
updateAndTouchUser updating time user =
  fromRec $ nub $ union (toRec $ updating) $ union (toRec $ keyed @"updated_at" time) (toRec user)
```

This function works as follows:

```haskell
> updateAndTouchUser updating time user
User {id = ID 42, email = "spider@man.com", username = "Spider Man", created_at = 2020-06-16 11:22:11.991147596 UTC, updated_at = 2020-06-16 11:31:35.170029827 UTC}
```

Note that using `nub` once after a chain of `union`s will be faster than using `nub`
after every individual `union`.

Wrapping up, we have done the following:

- Converting a record into its corresponding `Row` representation with the `toRow` function
- Adding, removing or replacing the fields over the `Row` with `union` and `nub`
- Converting back to a record with `fromRow`

## Row-polymorphic functions

The following `create` and `update` functions are generalized in terms of row-polymorphism.

```haskell
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
```

- `IsRow` is a constraint which defines the `Of` type family and a pair of
  `toRow` / `fromRow` functions.
  - `wakame` defines an instance of `IsRow` for all Haskell records with a `Generic` instance.
- `Lacks` constrains a row to not have a field with the given label.
- `Merge` is a combination of `Union` and `Nub`, which do appending and removing respectively.

With these constraints and functions, you can easily write row polymorphic
functions in your application.

These examples are found at [Wakame.Examples.Usage](https://github.com/kayhide/wakame/blob/master/test/examples/Wakame/Examples/Usage.hs).

There are other examples available at [Wakame.Examples.Functons](https://github.com/kayhide/wakame/blob/master/test/examples/Wakame/Examples/Functions.hs).

If you're interested in row polymorphism, the Wikipedia page may help: [Row polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism).


## Underlying data structure

`wakame` uses `NP` (a.k.a. "N-ary Product") as the underlying representation of
`Row`.  `NP` is a data type from the
[sop-core](https://hackage.haskell.org/package/sop-core) library.

So if you need finer control of `Row`, or if you need an advanced or
application-specific operation, you have the option of using the `NP` data type
directly, which will allow you to take advantage of the rich set of functions
from the `sop-core` library.

For more details, see the paper [True Sums Of Products](https://www.andres-loeh.de/TrueSumsOfProducts/).


### Why not `record-sop` ?

[records-sop](https://hackage.haskell.org/package/records-sop) is a library
built on top of `sop-core`.  It focuses on the representation of a record data
type and provides a set of functions for doing conversions.

The difference is that `records-sop` is more general, and also covers
functionality for non-record data types.  `wakame` is specialized for only
record data types.

Although the representation data types is virtually the same between
`records-sop` and `wakame`, how to convert between data types is different.

One of the benefits of `wakame` is the ability to introduce special conversion rules such as `keyed @"label" value` to / from `Row`.

`wakame` gives you the ability to make a single `keyed` value correspond to the
representation of a data type with one field, and any arbitrary tuple of
`keyed` values to a data type with multiple fields.  In this way, you can use a
tuple of `keyed` values in place of an anonymous record.

## What is wakame?

[Wakame](https://en.wikipedia.org/wiki/Wakame) is a type of edible seaweed, popular in Japan.

The most important property of wakame is that, it changes its color when boiled.

## Contributions

Feel free to open an issue or PR.
Thanks!
