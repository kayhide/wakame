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

To update a part of this record fields, define a data type corresponding to the updating fields as:

```haskell
data UpdatingUser =
  UpdatingUser
  { email :: Text
  , username :: Text
  }
  deriving Generic
```

and write a function as:


```haskell
updateUser :: UpdatingUser -> User -> User 
updateUser updating user = fromRec $ nub $ union (toRec updating) (toRec user)
```

Here is a working example of this function:

```haskell
> user
User {id = ID 42, email = "peter@amazing.com", username = "Peter Parker", created_at = 2020-06-16 11:22:11.991147596 UTC, updated_at = 2020-06-16 11:22:11.991147596 UTC}
> updating
UpdatingUser {email = "spider@man.com", username = "Spider Man"}
> updateUser updating user
User {id = ID 42, email = "spider@man.com", username = "Spider Man", created_at = 2020-06-16 11:22:11.991147596 UTC, updated_at = 2020-06-16 11:22:11.991147596 UTC}
```


To update the `updated_at` field, it can be done in the same manner.
But this time, let's do it without defining any record type:


```haskell
touchUser :: UTCTime -> User -> User
touchUser time user = fromRec $ nub $ union (toRec $ keyed @"updated_at" time) (toRec user)
```

So `toRec $ keyed @"update_at" time` creates the same `Row` object which has an only one field of:

```haskell
{ updated_at :: UTCTime }
```

And updating and touching can be done at one time as:

```haskell
updateAndTouchUser :: UpdatingUser -> UTCTime -> User -> User
updateAndTouchUser updating time user =
  fromRec $ nub $ union (toRec $ updating) $ union (toRec $ keyed @"updated_at" time) (toRec user)
```

Note that you can defer to `nub` until fully `union`ed to gain a performance benefit.

This function works as:

```haskell
> updateAndTouchUser updating time user
User {id = ID 42, email = "spider@man.com", username = "Spider Man", created_at = 2020-06-16 11:22:11.991147596 UTC, updated_at = 2020-06-16 11:31:35.170029827 UTC}
```

Wrapping up what is done here is:
- Converting a record into its corresponding `Row` representation by the calls of `toRow`
- Adding, removing or replacing the fields over the `Row` by `union` and `nub`
- Converting back to a record by `fromRow`


## Row-polymorphic functions

`create` and `update` functions are generalized in terms of row-polymorphism.

The following is an example:

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

- `IsRow` is a constraint which defines `Of` type family and a pair of `toRow` / `fromRow` functions.
  - `wakame` defineds an instance of `IsRow` for a normal Haskell record with `Generic` instance.
- `Lacks` constraints not to have a field with the given label.
- `Merge` is a combination of `Union` and `Nub`, which do appending and removing respectively.

Using these constraints and functions, row polymorphic functions are available in your application.

These examples are found at [Wakame.Examples.Usage](https://github.com/kayhide/wakame/blob/master/test/examples/Wakame/Examples/Usage.hs).

There are also some other examples available at [Wakame.Examples.Functons](https://github.com/kayhide/wakame/blob/master/test/examples/Wakame/Examples/Functions.hs).

As for the general idea of row polymorphism, the wikipedia page may help: [Row polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism).


## Underlying data structure

As a `Row` representation, `wakame` uses `NP` aka "N-ary Product", which is a data type come from [sop-core](https://hackage.haskell.org/package/sop-core) liberary.

So if any of finer, advanced or application-specific operation is wanted, it is an option to handle `NP` data directly taking advantage of rich set of functions from `sop-core` library.

For more details, see the paper [True Sums Of Products](https://www.andres-loeh.de/TrueSumsOfProducts/).


### Why not `record-sop` ?

[records-sop](https://hackage.haskell.org/package/records-sop) is a library built on top of `sop-core`, which focuses on the representation of a record data type and provides a set of conversion functionality.

The difference is that, `records-sop` is based on more general functionalities which also covers non-record data type, while `wakame` is specialized to a record data type only.

Though the representation data types are virtualy the same between `records-sop` and `wakame`, how to convert is different.

One of benefits of this doing is the ability to introduce a special conversion rule such as `keyed @"label" value` to / from `Row`.

`wakame` makes a single `keyed` value to correspond to a representation data with one field, and any arbitrary tuple of `keyed` values to multiple fields.
In this way, we can use a tuple of `keyed` values in place of an anonymous record.


## Contributions

Feel free to open an issue or PR.
Thanks!
