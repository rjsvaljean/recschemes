{-# LANGUAGE DeriveFunctor #-}

module Part4
  ( change,
    changeCombinations
  ) where

import           Prelude       hiding (lookup)

import           Control.Arrow
import qualified Data.Set      as Set
import qualified Data.List     as List (partition, sort)

newtype Term f = In { out :: f (Term f) }

data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)
              }

type CVAlgebra f a = f (Attr f a) -> a

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = out >>> fmap worker >>> h where
    worker t = Attr (histo h t) (fmap worker (out t))

type Cent = Int

coins :: [Cent]
coins = [50, 25, 10, 5, 1]

data Nat a
    = Zero
    | Next a
      deriving Functor

-- Convert from a natural number to its foldable equivalent, and vice versa.
expand :: Int -> Term Nat
expand 0 = In Zero
expand n = In (Next (expand (n - 1)))

compress :: Nat (Attr Nat a) -> Int
compress Zero              = 0
compress (Next (Attr _ x)) = 1 + compress x

change :: Cent -> Int
change amt = histo go (expand amt) where
  go :: Nat (Attr Nat Int) -> Int
  go Zero = 1
  go curr@(Next attr) = let
    given      = compress curr
    validCoins = filter (<= given) coins
    remaining  = map (given -) validCoins
    zeroCount  = length (filter (== 0) remaining)
    results    = sum (map (lookup attr) remaining)
    in zeroCount + results

changeCombinations :: Cent -> Set.Set [Cent]
changeCombinations amt = histo go (expand amt) where
  go :: Nat (Attr Nat (Set.Set [Cent])) -> Set.Set [Cent]
  go Zero = Set.empty
  go curr @(Next ann) = Set.union singletonChange others where
    (singletonChange, others) = (
      Set.map (const [given]) zeroes,
      Set.unions(map lkp (Set.toList nonZeroes))) where
      lkp (subtracted, i) = Set.map List.sort . Set.map (++ [subtracted]) $ lookup ann i
      (zeroesList, nonZeroesList) = List.partition (\x -> snd x == 0) remaining where
        remaining = map (\c -> (c, given - c)) validCoins where
          validCoins = filter (<= given) coins
      (zeroes, nonZeroes) = (Set.fromList zeroesList, Set.fromList nonZeroesList)
      given = compress curr

lookup :: Attr Nat a -> Int -> a
lookup cache n = last $ go [] cache where
  go :: [a] -> Attr Nat a -> [a]
  go window Attr{hole = Zero} = window
  go window Attr{hole = (Next tail), attribute = a} =
    go (newWindow (length window)) tail where
      newWindow l
        | l < n = a : window
        | l == n && l /= 0 = a : (init window)
        | otherwise = window
