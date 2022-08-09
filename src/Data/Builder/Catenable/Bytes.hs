{-# language BangPatterns #-}
{-# language PatternSynonyms #-}

-- | @Data.Builder.Bytes@ specialized to @Bytes@.
module Data.Builder.Catenable.Bytes
  ( -- * Type
    Builder(..)
    -- * Convenient infix operators
  , pattern (:<)
  , pattern (:>)
    -- * Run
  , run
  ) where

import Control.Monad.ST (ST,runST)
import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks(ChunksNil))

import qualified Data.Bytes.Builder as BB
import qualified Data.Bytes.Builder.Unsafe as BBU

infixr 5 :<
infixl 5 :>

data Builder
  = Empty
  | Cons {-# UNPACK #-} !Bytes !Builder
  | Snoc !Builder {-# UNPACK #-} !Bytes
  | Append !Builder !Builder

instance Monoid Builder where
  {-# inline mempty #-}
  mempty = Empty

instance Semigroup Builder where
  {-# inline (<>) #-}
  (<>) = Append

pattern (:<) :: Bytes -> Builder -> Builder
pattern (:<) x y = Cons x y

pattern (:>) :: Builder -> Bytes -> Builder
pattern (:>) x y = Snoc x y

run :: Builder -> Chunks
{-# noinline run #-}
run b = runST $ do
  bldr0 <- BBU.newBuilderState 128
  bldr1 <- pushCatenable bldr0 b
  BBU.reverseCommitsOntoChunks ChunksNil (BBU.closeBuilderState bldr1)

pushCatenable :: BBU.BuilderState s -> Builder -> ST s (BBU.BuilderState s)
pushCatenable !bldr0 b = case b of
  Empty -> pure bldr0
  Cons x b1 -> do
    bldr1 <- BBU.pasteST (BB.bytes x) bldr0
    pushCatenable bldr1 b1
  Snoc b1 x -> do
    bldr1 <- pushCatenable bldr0 b1
    BBU.pasteST (BB.bytes x) bldr1
  Append x y -> do
    bldr1 <- pushCatenable bldr0 x
    pushCatenable bldr1 y


