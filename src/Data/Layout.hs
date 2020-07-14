{-# LANGUAGE DerivingVia #-}

module Data.Layout (Layout (..), empty, (<>), fold, foldMap, times, Builder, build, toBuilder) where

import Prelude hiding ((<>), foldMap, replicate)
import Data.Bits (Bits (..))
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.Foldable (foldl')
import Data.Function (on)
import Data.Monoid (Alt (..))
import Numeric.Natural
import Util

data Layout a = Layout { size :: a, logAlign :: a }
  deriving (Eq, Show)

empty :: Num a => Layout a
empty = Layout { size = 0, logAlign = 0 }

(<>) :: (Integral a, Bits a) => Layout a -> Layout a -> Layout a
a <> b = Layout
  { logAlign = on max logAlign a b
  , size = roundUpToExp (fi $ logAlign b) (size a) + size b
  }

fold :: (Foldable f, Integral a, Bits a) => f (Layout a) -> Layout a
fold = foldl' (<>) empty

foldMap :: (Foldable f, Integral b, Bits b) => (a -> Layout b) -> f a -> Layout b
foldMap f = foldl' ((. f) . (<>)) empty

times :: (Integral a, Bits a) => Natural -> Layout a -> Layout a
times = (foldr (<>) empty :: [_] -> _) ∘∘ replicate

roundUpToExp logA = (+ (shiftL 1 logA - 1)) & (.&. complement (shiftL 1 logA))

newtype Builder a = Builder { unBuilder :: DList (Layout a) }
  deriving (Semigroup, Monoid) via (Alt DList (Layout a))

toBuilder :: Layout a -> Builder a
toBuilder = Builder . DList.singleton

build :: (Integral a, Bits a) => Builder a -> Layout a
build = fold . unBuilder

fi = fromIntegral
