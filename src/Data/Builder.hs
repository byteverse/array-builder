{-# language RankNTypes #-}
{-# language BangPatterns #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}

module Data.Builder
  ( -- * Builder
    Builder(..)
  , cons
  , singleton
  , doubleton
  , tripleton
    -- * Run
  , run
  ) where

import Compat (unsafeShrinkAndFreeze#)
import Data.Chunks (Chunks(ChunksNil,ChunksCons))
import Data.Primitive (SmallArray(SmallArray))
import GHC.Exts ((*#),(+#),(-#),(>#))
import GHC.Exts (SmallMutableArray#)
import GHC.Exts (State#,Int#,runRW#)
import GHC.Exts (newSmallArray#)
import GHC.Exts (writeSmallArray#,unsafeFreezeSmallArray#)

import qualified Data.Chunks as C

-- | Builder for an array of boxed elements.
newtype Builder a = Builder
  -- The chunks being built up are in reverse order.
  -- Consequently, functions that run a builder must
  -- reverse the chunks at the end.
  (forall s. SmallMutableArray# s a -> Int# -> Int# -> Chunks a -> State# s
   -> (# State# s, SmallMutableArray# s a, Int#, Int#, Chunks a #)
  )

run :: Builder a -> Chunks a
run (Builder f) = case runRW#
  -- The initial size of 16 elements is chosen somewhat
  -- arbitrarily. It is more than enough to saturate a
  -- cache line.
  (\s0 -> case newSmallArray# 16# errorThunk s0 of
    (# s1, marr0 #) -> case f marr0 0# 16# ChunksNil s1 of
      (# s2, marr, off, _, cs #) ->
        -- Recall that freezeSmallArray copies a slice.
        -- If resize functions ever become available for
        -- SmallArray, we should use that instead.
        case unsafeShrinkAndFreeze# marr off s2 of
          (# s3, arr #) ->
            let !r = C.reverseOnto
                  (ChunksCons (SmallArray arr) ChunksNil)
                  cs
             in (# s3, r #)
  ) of (# _, cs #) -> cs

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "array-builder:Data.Builder: error"

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

cons :: a -> Builder a -> Builder a
{-# inline cons #-}
cons a b = singleton a <> b

-- | A builder with one element.
singleton :: a -> Builder a
{-# noinline singleton #-}
singleton a = Builder
  (\marr off len cs s0 -> case len ># 0# of
    1# -> case writeSmallArray# marr off a s0 of
      s1 -> (# s1, marr, off +# 1#, len -# 1#, cs #)
    _ -> case unsafeFreezeSmallArray# marr s0 of
      (# s1, arr #) -> let !lenNew = nextLength off in
        -- Since we feed the element to newSmallArray#, we do not
        -- need to write it to the 0 index.
        case newSmallArray# lenNew a s1 of
          (# s2, marrNew #) ->
            let !csNew = ChunksCons (SmallArray arr) cs in
              (# s2, marrNew, 1#, lenNew -# 1#, csNew #)
  )

-- | A builder with two elements.
--
-- @since 0.1.1.0
doubleton :: a -> a -> Builder a
{-# noinline doubleton #-}
doubleton a b = Builder
  (\marr off len cs s0 -> case len ># 1# of
    1# -> case writeSmallArray# marr off a s0 of
      s1 -> case writeSmallArray# marr (off +# 1#) b s1 of
        s2 -> (# s2, marr, off +# 2#, len -# 2#, cs #)
    _ -> case unsafeShrinkAndFreeze# marr off s0 of
      (# s1, arr #) -> let !lenNew = nextLength off in
        -- Since we feed the element to newSmallArray#, we do not
        -- need to write element a to the 0 index.
        case newSmallArray# lenNew a s1 of
          (# s2, marrNew #) -> case writeSmallArray# marrNew 1# b s2 of
            s3 -> let !csNew = ChunksCons (SmallArray arr) cs in
              (# s3, marrNew, 2#, lenNew -# 2#, csNew #)
  )

-- | A builder with three elements.
--
-- @since 0.1.1.0
tripleton :: a -> a -> a -> Builder a
{-# noinline tripleton #-}
tripleton a b c = Builder
  (\marr off len cs s0 -> case len ># 1# of
    1# -> case writeSmallArray# marr off a s0 of
      s1 -> case writeSmallArray# marr (off +# 1#) b s1 of
        s2 -> case writeSmallArray# marr (off +# 2#) c s2 of
          s3 -> (# s3, marr, off +# 3#, len -# 3#, cs #)
    _ -> case unsafeShrinkAndFreeze# marr off s0 of
      (# s1, arr #) -> let !lenNew = nextLength off in
        -- Since we feed the element to newSmallArray#, we do not
        -- need to write element a to the 0 index.
        case newSmallArray# lenNew a s1 of
          (# s2, marrNew #) -> case writeSmallArray# marrNew 1# b s2 of
            s3 -> case writeSmallArray# marrNew 2# c s3 of
              s4 -> let !csNew = ChunksCons (SmallArray arr) cs in
                (# s4, marrNew, 3#, lenNew -# 3#, csNew #)
  )

nextLength :: Int# -> Int#
{-# inline nextLength #-}
nextLength i = i *# 2#
