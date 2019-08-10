{-# language RankNTypes #-}
{-# language BangPatterns #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}

module Data.Builder
  ( -- * Builder
    Builder(..)
  , singleton
    -- * Run
  , run
  , flatten
  , ReversedChunks(..)
  ) where

import Data.Primitive (SmallArray(SmallArray))
import Control.Monad.ST.Run (runSmallArrayST)
import GHC.Exts (State#,Int#,runRW#)
import GHC.Exts (writeSmallArray#,unsafeFreezeSmallArray#)
import GHC.Exts (SmallMutableArray#,freezeSmallArray#)
import GHC.Exts (newSmallArray#,sizeofSmallArray#)
import GHC.Exts ((*#),(+#),(-#),(>#))

import qualified Data.Foldable as F
import qualified Data.Primitive as PM

-- | Builder for an array of boxed elements. 
newtype Builder a = Builder
  (forall s. SmallMutableArray# s a -> Int# -> Int# -> ReversedChunks a -> State# s
   -> (# State# s, SmallMutableArray# s a, Int#, Int#, ReversedChunks a #)
  )

-- | A chunked array. The chunks are in reverse order,
-- but the elements within each chunk are in order.
-- The 'Foldable' instance iterates over the chunks in
-- reverse in order to provide the behavior that a user
-- would expect.
data ReversedChunks a
  = ReversedChunksCons !(SmallArray a) !(ReversedChunks a)
  | ReversedChunksNil

instance Foldable ReversedChunks where
  foldr = revChunksFoldr
  foldr' = revChunksFoldr'
  length = revChunksLength 0

revChunksFoldr :: (a -> b -> b) -> b -> ReversedChunks a -> b
{-# inline revChunksFoldr #-}
revChunksFoldr f = go where
  go acc ReversedChunksNil = acc
  go acc (ReversedChunksCons c cs) = 
    go (F.foldr f acc c) cs

revChunksFoldr' :: (a -> b -> b) -> b -> ReversedChunks a -> b
{-# inline revChunksFoldr' #-}
revChunksFoldr' f = go where
  go !acc ReversedChunksNil = acc
  go !acc (ReversedChunksCons c cs) = 
    go (F.foldr' f acc c) cs

run :: Builder a -> ReversedChunks a
run (Builder f) = case runRW#
  -- The initial size of 16 elements is chosen somewhat
  -- arbitrarily. It is more than enough to saturate a
  -- cache line.
  (\s0 -> case newSmallArray# 16# errorThunk s0 of
    (# s1, marr0 #) -> case f marr0 0# 16# ReversedChunksNil s1 of
      (# s2, marr, off, _, cs #) ->
        -- Recall that freezeSmallArray copies a slice.
        -- If resize functions ever become available for
        -- SmallArray, we should use that instead.
        case freezeSmallArray# marr 0# off s2 of
          (# s3, arr #) ->
            let !r = ReversedChunksCons (SmallArray arr) cs in
            (# s3, r #)
  ) of (# _, cs #) -> cs

flatten :: ReversedChunks a -> SmallArray a
flatten cs = runSmallArrayST $ do
  let !len = revChunksLength 0 cs
  !marr <- PM.newSmallArray len errorThunk
  -- TODO: write this
  -- let go 
  PM.unsafeFreezeSmallArray marr

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "array-builder:Data.Builder: error"

revChunksLength :: Int -> ReversedChunks a -> Int
revChunksLength !n ReversedChunksNil = n
revChunksLength !n (ReversedChunksCons c cs) =
  revChunksLength (n + PM.sizeofSmallArray c) cs

instance Monoid (Builder a) where
  {-# inline mempty #-}
  mempty = Builder
    (\marr0 off0 len0 cs0 s0 ->
      (# s0, marr0, off0, len0, cs0 #)
    )

instance Semigroup (Builder a) where
  {-# inline (<>) #-}
  Builder f <> Builder g = Builder
    (\marr0 off0 len0 cs0 s0 -> case f marr0 off0 len0 cs0 s0 of
      (# s1, marr1, off1, len1, cs1 #) ->
        g marr1 off1 len1 cs1 s1
    )

singleton :: a -> Builder a
{-# inline singleton #-}
singleton a = Builder
  (\marr off len cs s0 -> case len ># 0# of
    1# -> case writeSmallArray# marr off a s0 of
      s1 -> (# s1, marr, off +# 1#, len -# 1#, cs #)
    _ -> case unsafeFreezeSmallArray# marr s0 of
      (# s1, arr #) -> let !lenNew = nextLength (sizeofSmallArray# arr) in
        case newSmallArray# lenNew a s1 of
          (# s2, marrNew #) ->
            let !csNew = ReversedChunksCons (SmallArray arr) cs in
              (# s2, marrNew, 1#, lenNew -# 1#, csNew #)
  )

nextLength :: Int# -> Int#
{-# inline nextLength #-}
nextLength i = i *# 2#
