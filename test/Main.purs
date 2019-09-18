module Test.Main where

import Containers.Difference (Difference (..))
import Containers.Difference as DC

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.List.Lazy as LL
import Data.List as L
import Data.Maybe
import Partial.Unsafe (unsafePartial)
import Control.Monad.State
import Data.Array as A
import Data.Foldable
import Data.List.Lazy.Types
import Test.Assert
import Data.Unfoldable

foreign import getStackSize :: Unit -> Int


snoc :: forall list.
           Applicative list => Monoid (list Int) =>
           Int -> (list Int)
snoc n = go (pure n) n
  where
    go l 0 = l
    go l n = go (l <> pure n) (n - 1)


-- TODO: rewrite it in more elegant manner
testStackSafety :: Effect Unit
testStackSafety = do
  let stackSize = getStackSize unit
  let depth = stackSize * 2
  assertEqual { expected: Just 1
              , actual: L.last $ DC.toContainer $ snoc depth }
  -- node.js may increase stack size after hitting the limit once
  let stackSize' = getStackSize unit
  assert' "testStackSafety is correct" (stackSize' < depth)



assertEq :: forall a. Show a => Eq a => a -> a -> Effect Unit
assertEq e a = assertEqual { actual: a, expected: e }


main :: Effect Unit
main = do

  log "checking Semigroup instance"
  assertEq (DC.toContainer ((pure 1 <> pure 2) <> pure 3))
           (DC.toContainer (pure 1 <> (pure 2 <> pure 3)) :: L.List Int)
  assertEq (DC.toContainer ((pure 1 <> pure 2) <> (pure 3 <> pure 4)))
           (DC.toContainer (pure 1 <> (pure 2 <> pure 3) <> pure 4) :: L.List Int)
  assertEq (DC.toContainer (pure 1 <> (pure 2 <> (pure 3 <> pure 4))))
           (DC.toContainer (((pure 1 <> pure 2) <> pure 3) <> pure 4) :: L.List Int)
  assertEq (DC.toContainer (pure 1 <> mempty))
           (DC.toContainer (mempty <> pure 1) :: L.List Int)


  log "checking fromFoldable"
  assertEq (DC.fromFoldable' [1, 2, 3, 4] :: Difference List Int)
           (DC.fromFoldable' [1, 2, 3, 4])
  assertEq (DC.toContainer (DC.fromFoldable' [1, 2, 3, 4] :: Difference L.List Int))
           (L.fromFoldable                   [1, 2, 3, 4])
  assertEq (DC.toContainer (DC.fromFoldable' [] :: Difference L.List Int))
           (L.fromFoldable                   [])
  assertEq (DC.toContainer (pure 1 <> pure 2 <> pure 3 <> pure 4))
           (L.fromFoldable [1, 2, 3, 4])

  assertEq (DC.toContainer (DC.snoc (DC.cons 1 mempty) 2 :: Difference LL.List Int))
           (LL.snoc (LL.cons 1 mempty) 2)

  assertEq (pure 1 <> pure 2 <> pure 3 <> pure 4)
           ((DC.fromFoldable' [1,2,3,4] :: Difference L.List Int))
  assertEq ((pure 1 <> pure 2) <> pure 3 :: Difference L.List Int)
           (pure 1 <> (pure 2 <> pure 3))

  log "checking Monoid instance"
  assertEq (DC.toContainer $ DC.fromContainer (mempty :: L.List Unit))
           (L.Nil)
  assertEq (DC.toContainer $ DC.fromContainer (pure 1 <> mempty :: L.List Int))
           (L.fromFoldable [1])
  assertEq (DC.toContainer $ DC.fromContainer (mempty <> pure 2 :: L.List Int))
           (L.fromFoldable [2])

  log "checking Functor instance"
  assertEq (L.fromFoldable [10, 20, 30])
           (DC.toContainer $ map (_ * 10) $ DC.fromFoldable' [1, 2, 3])

  log "checking Unfoldable instance"
  assertEq (DC.toContainer (replicate 10 0 :: Difference L.List Int))
           (replicate 10 0)

  log "checking Foldable instance"
  assertEq 6 (foldr (\x y -> x + y) 0 (DC.fromFoldable' [1, 2, 3] :: Difference List Int))
  assertEq (foldr (\x y -> x - y) 0 (DC.fromFoldable' [1, 2, 3] :: Difference List Int))
           (foldr (\x y -> x - y) 0 [1, 2, 3])
  assertEq (foldl (\x y -> x - y) 0 (DC.fromFoldable' [1, 2, 3] :: Difference List Int))
           (foldl (\x y -> x - y) 0 [1, 2, 3])
  assertEq (foldMap pure (DC.fromFoldable' [1,2,3,4] :: Difference List Int))
           (foldMap pure [1,2,3,4] :: List Int)

  log "checking Monad instance"
  assertEq (bind (DC.fromFoldable' [1, 2, 3] :: Difference List Int) (\x -> pure x <> pure (x * 10)))
           (DC.fromFoldable' $ bind [1, 2, 3] (\x -> pure x <> pure (x * 10)) )

  log "checking that `append` is stack-safe (this may take some time)"
  testStackSafety
