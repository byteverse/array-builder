{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Compat
  ( unsafeShrinkAndFreeze
  , unsafeShrinkAndFreeze#
  ) where

import Data.Primitive (SmallArray(..),SmallMutableArray(..))
import GHC.Exts (SmallArray#,SmallMutableArray#,Int(I#))
import GHC.Exts (State#,Int#)
import GHC.ST (ST(ST))

import qualified GHC.Exts as Exts

-- Shrink the mutable array in place and then freeze it.
-- The argument must not be reused after being passed to
-- this function.
unsafeShrinkAndFreeze ::
     SmallMutableArray s a
  -> Int
  -> ST s (SmallArray a)
{-# inline unsafeShrinkAndFreeze #-}
unsafeShrinkAndFreeze (SmallMutableArray x) (I# n) = ST
  (\s0 -> case Exts.shrinkSmallMutableArray# x n s0 of
    s1 -> case Exts.unsafeFreezeSmallArray# x s1 of
      (# s2, r #) -> (# s2, SmallArray r #)
  )

unsafeShrinkAndFreeze# ::
     SmallMutableArray# s a
  -> Int#
  -> State# s
  -> (# State# s, SmallArray# a #)
{-# inline unsafeShrinkAndFreeze# #-}
unsafeShrinkAndFreeze# x n s0 =
  case Exts.shrinkSmallMutableArray# x n s0 of
    s1 -> Exts.unsafeFreezeSmallArray# x s1
