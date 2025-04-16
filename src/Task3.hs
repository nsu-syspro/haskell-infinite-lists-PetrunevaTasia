{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}
-- The above pragma enables all warnings

module Task3 where

import Task2 (Stream (..), fromList, showHelper)
import Data.Ratio (Ratio)
import Data.Foldable(toList)

instance Foldable Series where
  foldMap :: Monoid m => (a -> m) -> Series a -> m
  foldMap f = foldr (\a m -> f a <> m) mempty

  toList :: Series a -> [a]
  toList (Series a) = toList a 

instance Show a => Show (Series a) where
  show :: Show a => Series a -> String
  show (Series stream) = showHelper 0 stream

-- | Power series represented as infinite stream of coefficients
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (x + x ^ 2 + x ^ 4)
-- [0,1,1,0,1,0,0,0,0,0]
-- >>> coefficients ((1 + x)^5)
-- [1,5,10,10,5,1,0,0,0,0]
-- >>> coefficients (42 :: Series Integer)
-- [42,0,0,0,0,0,0,0,0,0]
--
newtype Series a = Series
  { coefficients :: Stream a
  -- ^ Returns coefficients of given power series
  --
  -- For following series
  --   @a0 + a1 * x + a2 * x^2 + ...@
  -- coefficients would be
  --   @a0, a1, a2, ...@
  }

-- | Power series corresponding to single @x@
--
-- First 10 coefficients:
--
-- >>> coefficients x
-- [0,1,0,0,0,0,0,0,0,0]
--
x :: Num a => Series a
x = Series (fromList 0 [0, 1])

instance (Num a) => Num (Series a) where

    fromInteger :: Num a => Integer -> Series a
    fromInteger n = Series (fromList 0 [fromInteger n])

    negate :: Num a => Series a -> Series a
    negate (Series stream) = Series (fmap (* (-1)) stream)

    (+) :: Num a => Series a -> Series a -> Series a
    (+) (Series a) (Series b) = Series (plusInStreams a b)

    (*) :: Num a => Series a -> Series a -> Series a
    (*) (Series stream1) (Series stream2) = Series (mulInStreams stream1 stream2)

    abs :: Num a => Series a -> Series a
    abs (Series a) = Series (fmap abs a)

    signum :: Num a => Series a -> Series a
    signum (Series a) = Series (fmap signum a)

plusInStreams :: Num a => Stream a -> Stream a -> Stream a
plusInStreams (Stream f1 s1) (Stream f2 s2) = Stream (f1 + f2) (plusInStreams s1 s2)

minusInStreams :: Num a => Stream a -> Stream a -> Stream a
minusInStreams (Stream f1 s1) (Stream f2 s2) = Stream (f1 - f2) (minusInStreams s1 s2)

mulInStreams :: Num a => Stream a -> Stream a -> Stream a
mulInStreams (Stream f1 s1) (Stream f2 s2) = 
  Stream (f1 * f2) (fmap (f1 *) s2 `plusInStreams` (s1 `mulInStreams` Stream f2 s2))

-- | Multiplies power series by given number
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (2 *: (x + x ^ 2 + x ^ 4))
-- [0,2,2,0,2,0,0,0,0,0]
-- >>> coefficients (2 *: ((1 + x)^5))
-- [2,10,20,20,10,2,0,0,0,0]
--
infixl 7 *:
(*:) :: Num a => a -> Series a -> Series a
(*:) val (Series stream) = Series(fmap (val *) stream)



instance (Fractional a) => Fractional (Series a) where

  fromRational :: Fractional a => Rational -> Series a
  fromRational n = Series (fromList 0 [fromRational n])

  (/) :: Fractional a => Series a -> Series a -> Series a
  (/) (Series stream1) (Series stream2) = Series (divInStreams stream1 stream2)

divInStreams :: Fractional a => Stream a -> Stream a -> Stream a
divInStreams (Stream f1 s1) (Stream f2 s2) = 
  Stream q ((s1 `minusInStreams` fmap (q *) s2) `divInStreams` Stream f2 s2)
  where q = f1 / f2

-- | Helper function for producing integer
-- coefficients from generating function
-- (assuming denominator of 1 in all coefficients)
--
-- Usage example:
--
-- >>> gen $ (2 + 3 * x)
-- [2,3,0,0,0,0,0,0,0,0]
--
gen :: Series (Ratio Integer) -> Stream Integer
gen (Series stream) = coefficients (Series (fmap round stream))

-- | Returns infinite stream of ones
--
-- First 10 elements:
--
-- >>> ones
-- [1,1,1,1,1,1,1,1,1,1]
--
ones :: Stream Integer
ones = gen (1 / (1 - x))

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = gen (1 / (1 - 2 * x + x * x))

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = gen (x / (1 - x - x * x))