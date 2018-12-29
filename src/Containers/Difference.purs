module Containers.Difference
       ( Difference (..)
       , class Diff, dempty, dappend, dsingleton
       , fromFoldable
       , fromFoldable'
       , toContainer
       , fromContainer
       , singleton
       , cons
       , snoc
       ) where

import Prelude ( class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad
               , class Monoid, class Ord, class Semigroup, class Show, ap, append, bind, compare
               , eq, map, mempty, pure, show, (<>), (>>>))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Traversable (class Traversable, sequence)
import Data.List.Lazy as LL
import Data.List as L
import Data.Function (apply)
import Data.NaturalTransformation (type (~>))


-- | `Difference cnt e` is a data structure analogous to difference lists,
-- | where `cnt` is a container type, e.g. `List` from `Data.List.Lazy`,
-- | and `e` is an element type.
-- |
-- | ```
-- | newtype Difference cnt e = Difference (Array (cnt e -> cnt e))
-- | ```
newtype Difference cnt e = Difference (Array (cnt e -> cnt e))

-- | `Diff` typeclass generalizes difference lists.
-- |
-- | - `dempty` is an empty structure used for unfolding `Difference cnt e`.
-- | - `dappend` is an associative operation with time complexity that depends on the first argument's
-- |    size more than on the size of the second.
-- | - `dsingleton` should wrap one element into the container.
class (Foldable cnt) <= Diff cnt where
  dempty :: forall a. cnt a
  dappend :: forall a. cnt a -> cnt a -> cnt a
  dsingleton :: forall a. a -> cnt a

instance semigroupDifference :: Diff cnt => Semigroup (Difference cnt e) where
  append (Difference xs) (Difference ys) = Difference (xs <> ys)

instance monoidDifference :: Diff cnt => Monoid (Difference cnt e) where
  mempty = Difference []

instance foldableDifference :: Diff cnt => Foldable (Difference cnt) where
  foldr f b = toContainer >>> foldr f b
  foldl f b = toContainer >>> foldl f b
  foldMap f = toContainer >>> foldMap f

instance unfoldable1Difference :: (Diff cnt, Unfoldable1 cnt) => Unfoldable1 (Difference cnt) where
  unfoldr1 f = unfoldr1 f >>> fromContainer

instance unfoldableDifference :: (Diff cnt, Unfoldable cnt) => Unfoldable (Difference cnt) where
  unfoldr f = unfoldr f >>> fromContainer

instance traversableDifference :: (Diff cnt, Traversable cnt) => Traversable (Difference cnt) where
  traverse f = toContainer >>> map f >>> sequence >>> map fromContainer
  sequence = toContainer >>> sequence >>> map fromContainer

instance functorDifference :: (Diff cnt, Functor cnt) => Functor (Difference cnt) where
  -- We have no option other than to flatten the array.
  -- The reason is that it isn't possible to map 'Array (cnt a -> cnt a)` to `Array (cnt b -> cnt b)`
  map f = toContainer >>> map f >>> fromContainer

instance applyDifference :: (Diff cnt, Monad cnt) => Apply (Difference cnt) where
  apply = ap

instance bindDifference :: (Diff cnt, Monad cnt) => Bind (Difference cnt) where
  bind m k = fromContainer (bind (toContainer m) (k >>> toContainer))

instance applicativeDifference :: (Diff cnt, Monad cnt) => Applicative (Difference cnt) where
  pure = singleton

instance monadDifference :: (Diff cnt, Monad cnt) => Monad (Difference cnt)

instance showDifference :: (Show a, Show (cnt a), Diff cnt) => Show (Difference cnt a) where
  show d = "fromContainer (" <> show (toContainer d) <> ")"

instance eqDifference :: (Monoid (cnt a), Diff cnt, Eq (cnt a)) => Eq (Difference cnt a) where
  eq d1 d2 = eq (toContainer d1) (toContainer d2)

instance ordDifference :: (Monoid (cnt a), Diff cnt, Ord (cnt a)) => Ord (Difference cnt a) where
  compare d1 d2 = compare (toContainer d1) (toContainer d2)


fromFoldable :: forall f.
                Diff f => Foldable f =>
                f ~> Difference f
fromFoldable = fromFoldable'


-- | For any `Foldable` container, construct any other `Diff` container.
fromFoldable' :: forall f d.
                -- That's rather surprising that we can change containers on the fly.
                Diff d => Foldable f =>
                f ~> Difference d
fromFoldable' = foldMap singleton


fromContainer :: forall cnt. Diff cnt => cnt ~> Difference cnt
fromContainer l = Difference [dappend l]


toContainer :: forall cnt. Diff cnt => Difference cnt ~> cnt
toContainer = unDiff dempty
  where
    unDiff l (Difference []) = l
    unDiff l (Difference fs) = foldr apply l fs


singleton :: forall cnt a. Diff cnt =>
             a -> Difference cnt a
singleton = dsingleton >>> fromContainer


-- | *O(1)*. Prepend a single element.
cons :: forall cnt a. Diff cnt =>
        a -> Difference cnt a -> Difference cnt a
cons x xs = singleton x <> xs


-- | *O(1)*. Append a single element.
snoc :: forall cnt a. Diff cnt => Difference cnt a -> a -> Difference cnt a
snoc xs x = xs <> singleton x


instance diffListLazy :: Diff LL.List where
  dempty = mempty
  dappend = append
  dsingleton = pure

instance diffList :: Diff L.List where
  dempty = mempty
  dappend = append
  dsingleton = pure

instance diffArray :: Diff Array where
  dempty = mempty
  dappend = append
  dsingleton = pure
