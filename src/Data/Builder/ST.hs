{-# language BangPatterns #-}

module Data.Builder.ST
  ( Builder(..)
  , new
  , push
  , freeze 
  ) where

import Data.Primitive (SmallMutableArray)
import Control.Monad.ST (ST)
import Data.Primitive (newSmallArray,writeSmallArray,unsafeFreezeSmallArray)
import Data.Primitive (sizeofSmallArray,freezeSmallArray)
import Data.Chunks (Chunks(ChunksNil,ChunksCons))
import Foreign.Storable (sizeOf)

import qualified Data.Chunks as C

-- | Builder for an array of boxed elements. This type is appropriate
-- when constructing an array of unknown size in an effectful
-- (@ST@ or @IO@) setting. In a non-effectful setting, consider
-- the @Builder@ from @Data.Builder@ instead.
--
-- A 'Builder' must be used linearly. The type system does not
-- enforce this, so users must be careful when handling a 'Builder'.
data Builder s a = Builder
  !(SmallMutableArray s a)
  !Int
  !Int
  !(Chunks a)

-- | Create a new 'Builder' with no elements in it.
new :: ST s (Builder s a)
new = do
  marr <- newSmallArray initialLength errorThunk
  pure (Builder marr 0 initialLength ChunksNil)

-- | Push an element onto the end of the builder. This
-- is not strict in the element, so force it before pushing
-- it on to the builder if doing so is needed to prevent
-- space leaks.
push ::
     a -- ^ Element to push onto the end
  -> Builder s a -- ^ Builder, do not reuse this after pushing onto it
  -> ST s (Builder s a) -- ^ New builder
push a (Builder marr off len cs) = case len > 0 of
  True -> do
    writeSmallArray marr off a
    pure $! Builder marr (off + 1) (len - 1) cs
  False -> do
    arr <- unsafeFreezeSmallArray marr
    let lenNew = nextLength (sizeofSmallArray arr)
    marrNew <- newSmallArray lenNew a
    let !csNew = ChunksCons arr cs
    pure $! Builder marrNew 1 (lenNew - 1) csNew

-- The sequence of sizes we create is:
--   64-bit: 14, 30, 62, 126, 254, 254, 254...
--   32-bit: 14, 30, 62, 126, 254, 510, 510, 510...
-- The goal is to have objects whose sizes are increasing
-- powers of 2 until we reach the size of a block (4KB).
-- A 254-element SmallArray on a 64-bit platform uses
-- exactly 4KB (header + ptrs + payload).
nextLength :: Int -> Int
nextLength i = if i < maxElementCount - smallArrayHeaderWords
  then i * 2 + smallArrayHeaderWords
  else maxElementCount - smallArrayHeaderWords

maxElementCount :: Int
maxElementCount = div 4096 (sizeOf (undefined :: Int))

initialLength :: Int
initialLength = 16 - smallArrayHeaderWords

smallArrayHeaderWords :: Int
smallArrayHeaderWords = 2

-- | Convert a 'Builder' to 'Chunks'. The 'Builder' must not
-- be reused after this operation.
freeze ::
     Builder s a -- ^ Builder, do not reuse after freezing
  -> ST s (Chunks a)
freeze (Builder marr off _ cs) = do
  arr <- freezeSmallArray marr 0 off
  pure $! C.reverseOnto (ChunksCons arr ChunksNil) cs

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "array-builder:Data.Builder.ST: error"

