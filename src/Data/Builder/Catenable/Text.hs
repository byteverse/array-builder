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
    -- * Properties
  , length
    -- * Create
  , shortText
  , char
  , word32Dec
  , word64Dec
  , int32Dec
  , int64Dec
  ) where

import Prelude hiding (length)

import Control.Monad.ST (ST,runST)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Bytes.Chunks (Chunks(ChunksNil))
import Data.Int (Int32,Int64)
import Data.Primitive (ByteArray(ByteArray))
import Data.String (IsString(fromString))
import Data.Text.Short (ShortText)
import Data.Word (Word32,Word64)

import qualified Arithmetic.Nat as Nat
import qualified Data.Bytes.Builder as BB
import qualified Data.Bytes.Builder.Bounded as Bounded
import qualified Data.Bytes.Builder.Unsafe as BBU
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS

infixr 5 :<
infixl 5 :>

data Builder
  = Empty
  | Cons !ShortText !Builder
  | Snoc !Builder !ShortText
  | Append !Builder !Builder

shortText :: ShortText -> Builder
shortText !t = Cons t Empty

char :: Char -> Builder
char !c = Cons (TS.singleton c) Empty

word32Dec :: Word32 -> Builder
word32Dec !i = Cons (ba2st (Bounded.run Nat.constant (Bounded.word32Dec i))) Empty

word64Dec :: Word64 -> Builder
word64Dec !i = Cons (ba2st (Bounded.run Nat.constant (Bounded.word64Dec i))) Empty

int32Dec :: Int32 -> Builder
int32Dec !i = Cons (ba2st (Bounded.run Nat.constant (Bounded.int32Dec i))) Empty

int64Dec :: Int64 -> Builder
int64Dec !i = Cons (ba2st (Bounded.run Nat.constant (Bounded.int64Dec i))) Empty

-- | Number of Unicode code points in the sequence.
length :: Builder -> Int
length b0 = case b0 of
  Empty -> 0
  Cons x b1 -> TS.length x + length b1
  Snoc b1 x -> TS.length x + length b1
  Append x y -> length x + length y

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

-- | Not structural equality. Converts builders to chunks and then
-- compares the chunks.
instance Eq Builder where
  a == b = run a == run b

instance Show Builder where
  show b = TS.unpack (ba2st (Chunks.concatU (run b)))

ba2st :: ByteArray -> ShortText
{-# inline ba2st #-}
ba2st (ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)

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
