{-# language BangPatterns #-}
{-# language PatternSynonyms #-}

-- | @Data.Builder.Catenable@ specialized to @ShortText@.
module Data.Builder.Catenable.Text
  ( -- * Type
    Builder(..)
    -- * Convenient infix operators
  , pattern (:<)
  , pattern (:>)
    -- * Run
  , run
  ) where

import Control.Monad.ST (ST,runST)
import Data.Text.Short (ShortText)
import Data.Bytes.Chunks (Chunks(ChunksNil))
import Data.String (IsString(fromString))

import qualified Data.Text.Short as TS
import qualified Data.Bytes.Builder as BB
import qualified Data.Bytes.Builder.Unsafe as BBU

infixr 5 :<
infixl 5 :>

data Builder
  = Empty
  | Cons !ShortText !Builder
  | Snoc !Builder !ShortText
  | Append !Builder !Builder

-- | Note: The choice of appending to the left side of @Empty@ instead
-- of the right side of arbitrary. Under ordinary use, this difference
-- cannot be observed by the user.
instance IsString Builder where
  fromString t = Cons (TS.pack t) Empty

instance Monoid Builder where
  {-# inline mempty #-}
  mempty = Empty

instance Semigroup Builder where
  {-# inline (<>) #-}
  (<>) = Append

pattern (:<) :: ShortText -> Builder -> Builder
pattern (:<) x y = Cons x y

pattern (:>) :: Builder -> ShortText -> Builder
pattern (:>) x y = Snoc x y

-- | The result is chunks, but this is guaranteed to be UTF-8 encoded
-- text, so if needed, you can flatten out the chunks and convert back
-- to @ShortText@.
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
    bldr1 <- BBU.pasteST (BB.shortTextUtf8 x) bldr0
    pushCatenable bldr1 b1
  Snoc b1 x -> do
    bldr1 <- pushCatenable bldr0 b1
    BBU.pasteST (BB.shortTextUtf8 x) bldr1
  Append x y -> do
    bldr1 <- pushCatenable bldr0 x
    pushCatenable bldr1 y

