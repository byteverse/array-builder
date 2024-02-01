{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Compat
  ( unsafeShrinkAndFreeze
  , unsafeShrinkAndFreeze#
  ) where

import Control.Monad.ST (ST)
import Data.Primitive (SmallArray, SmallMutableArray)
import GHC.Exts (Int#, SmallArray#, SmallMutableArray#, State#)

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- Shrink the mutable array in place and then freeze it.
-- The argument must not be reused after being passed to
-- this function.
unsafeShrinkAndFreeze ::
  SmallMutableArray s a ->
  Int ->
  ST s (SmallArray a)
{-# INLINE unsafeShrinkAndFreeze #-}
unsafeShrinkAndFreeze arr = PM.freezeSmallArray arr 0

unsafeShrinkAndFreeze# ::
  SmallMutableArray# s a ->
  Int# ->
  State# s ->
  (# State# s, SmallArray# a #)
{-# INLINE unsafeShrinkAndFreeze# #-}
unsafeShrinkAndFreeze# x n s0 =
  Exts.freezeSmallArray# x 0# n s0
