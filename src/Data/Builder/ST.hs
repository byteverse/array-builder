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

import qualified Data.Chunks as C

data Builder s a = Builder
  !(SmallMutableArray s a)
  !Int
  !Int
  !(Chunks a)

new :: ST s (Builder s a)
new = do
  marr <- newSmallArray 16 errorThunk
  pure (Builder marr 0 16 ChunksNil)

-- | Push an element onto the front of the builder. This 
push ::
     a -- ^ Element to push onto the front
  -> Builder s a -- ^ Builder, do not reuse this after pushing onto it
  -> ST s (Builder s a) -- ^ New builder
{-# inline push #-}
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
  
nextLength :: Int -> Int
{-# inline nextLength #-}
nextLength i = i * 2

freeze ::
     Builder s a -- ^ Builder, do not reuse after freezing
  -> ST s (Chunks a)
freeze (Builder marr off _ cs) = do
  arr <- freezeSmallArray marr 0 off
  pure $! C.reverseOnto (ChunksCons arr ChunksNil) cs

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "array-builder:Data.Builder.ST: error"

