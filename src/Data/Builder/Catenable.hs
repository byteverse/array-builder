{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- | Builder with cheap concatenation. Like the builder type from
@Data.Builder.ST@, this builder can be stored somewhere and this used
again later. However, this builder type has several advantages:

* Supports both cons and snoc (@Data.Builder.ST@ only supports snoc)
* No linear-use restriction
* Extremely cheap concatenation (not supported by @Data.Builder.ST@ at all)

In exchange for all of these, this implementation trades performance.
Performance is degraded for two reasons:

* Evaluation of the builder is deferred, and the evaluation requires walking
  a tree of nodes.
* This builder stores individual elements rather than chunks. There is
  no fundamental reason for this. It is possible to store a SmallArray
  in each Cons and Snoc instead, but this makes the implementation a
  little more simple.

One reason to prefer this module instead of @Data.Builder.ST@ is that
this module lets the user works with builder in a more monoidal style
rather than a stateful style. Consider a data type with several fields
that is being converted to a builder. Here, @Data.Builder.ST@
would require that @Builder@ appear as both an argument and an result for
each field\'s encode function. The linearly-used builder must be threaded
through by hand or by clever use of @StateT@. With @Data.Builder.Catenable@,
the encode functions only need return the builder.
-}
module Data.Builder.Catenable
  ( -- * Type
    Builder (..)

    -- * Convenient infix operators
  , pattern (:<)
  , pattern (:>)

    -- * Functions
  , singleton
  , doubleton
  , tripleton

    -- * Run
  , run
  ) where

import Control.Monad.ST (ST, runST)
import Data.Chunks (Chunks)
import Data.Foldable (foldl')
import GHC.Exts (IsList (..))

import qualified Data.Builder.ST as STB
import qualified Data.Chunks as Chunks

infixr 5 :<
infixl 5 :>

data Builder a
  = Empty
  | Cons a !(Builder a)
  | Snoc !(Builder a) a
  | Append !(Builder a) !(Builder a)

deriving stock instance Functor Builder

instance Monoid (Builder a) where
  {-# INLINE mempty #-}
  mempty = Empty

instance Semigroup (Builder a) where
  {-# INLINE (<>) #-}
  (<>) = Append

instance IsList (Builder a) where
  type Item (Builder a) = a
  toList = toList . Chunks.concat . run
  fromList = foldl' (\acc x -> acc :> x) Empty

pattern (:<) :: a -> Builder a -> Builder a
pattern (:<) x y = Cons x y

pattern (:>) :: Builder a -> a -> Builder a
pattern (:>) x y = Snoc x y

run :: Builder a -> Chunks a
{-# NOINLINE run #-}
run b = runST $ do
  bldr0 <- STB.new
  bldr1 <- pushCatenable bldr0 b
  STB.freeze bldr1

pushCatenable :: STB.Builder s a -> Builder a -> ST s (STB.Builder s a)
pushCatenable !bldr0 b = case b of
  Empty -> pure bldr0
  Cons x b1 -> do
    bldr1 <- STB.push x bldr0
    pushCatenable bldr1 b1
  Snoc b1 x -> do
    bldr1 <- pushCatenable bldr0 b1
    STB.push x bldr1
  Append x y -> do
    bldr1 <- pushCatenable bldr0 x
    pushCatenable bldr1 y

singleton :: a -> Builder a
{-# INLINE singleton #-}
singleton a = Cons a Empty

doubleton :: a -> a -> Builder a
{-# INLINE doubleton #-}
doubleton a b = Cons a (Cons b Empty)

tripleton :: a -> a -> a -> Builder a
{-# INLINE tripleton #-}
tripleton a b c = Append (Cons a (Cons b Empty)) (Cons c Empty)
