module BenchMain where

import Containers.Difference (Difference (..))
import Containers.Difference as DC

import Prelude
import Effect (Effect)
import Data.List.Lazy as LL
import Data.List as L
import Data.Array as A

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Test.QuickCheck.Gen (stateful)



explode :: forall list.
           Applicative list => Monoid (list Unit) =>
           Int -> (list Unit)
explode n = go (pure unit) n
  where
    go l 0 = l
    go l n = go (l <> l) (n - 1)


benchExplode :: Benchmark
benchExplode = mkBenchmark
  { slug: "explode"
  , title: "Recursively duplicate list n times and get the last element"
  , sizes: (A.range 0 19)
  , sizeInterpretation: "n"
  , inputsPerSize: 1
  , gen: \n -> stateful (\seed -> pure n)
  , functions: [ benchFn "Lazy Difference List" (LL.last <<< DC.toContainer <<< (explode :: Int -> Difference LL.List Unit))
               , benchFn "Difference List" (L.last <<< DC.toContainer <<< (explode :: Int -> Difference L.List Unit))
               , benchFn "Lazy List"  (LL.last <<< explode :: Int -> LL.List Unit)
               , benchFn "List"  (L.last <<< explode :: Int -> L.List Unit)
               ]
  }


snoc :: forall list.
           Applicative list => Monoid (list Int) =>
           Int -> (list Int)
snoc n = go (pure n) n
  where
    go l 0 = l
    go l n = go (l <> pure n) (n - 1)


benchSnoc :: Benchmark
benchSnoc = mkBenchmark
  { slug: "snoc"
  , title: "Append element to the end n times and get the last element"
  , sizes: (A.range 1 50) <#> (_ * 2)
  , sizeInterpretation: "n"
  , inputsPerSize: 2
  , gen: \n -> stateful (\seed -> pure n)
  , functions: [ benchFn "Lazy Difference List" (LL.last <<< DC.toContainer <<< snoc :: Int -> Difference LL.List Int)
               , benchFn "Difference List" (L.last <<< DC.toContainer <<< snoc :: Int -> Difference L.List Int)
               , benchFn "Lazy List"  (LL.last <<< snoc :: Int -> LL.List Int)
               , benchFn "List"  (L.last <<< snoc :: Int -> L.List Int)
               ]
  }


cons :: forall list.
           Applicative list => Monoid (list Int) =>
           Int -> (list Int)
cons n = go (pure n) n
  where
    go l 0 = l
    go l n = go (pure n <> l) (n - 1)


benchCons :: Benchmark
benchCons = mkBenchmark
  { slug: "cons"
  , title: "Append element to the beginning n times and get the last element"
  , sizes: (A.range 1 50) <#> (_ * 2)
  , sizeInterpretation: "n"
  , inputsPerSize: 2
  , gen: \n -> stateful (\seed -> pure n)
  , functions: [ benchFn "Lazy Difference List" (LL.last <<< DC.toContainer <<< cons :: Int -> Difference LL.List Int)
               , benchFn "Difference List" (L.last <<< DC.toContainer <<< cons :: Int -> Difference L.List Int)
               , benchFn "Lazy List"  (LL.last <<< cons :: Int -> LL.List Int)
               , benchFn "List"  (L.last <<< cons :: Int -> L.List Int)
               ]
  }


main :: Effect Unit
main = do
  runSuite [ benchExplode ]
  runSuite [ benchSnoc ]
  runSuite [ benchCons ]
