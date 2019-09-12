{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Data.Builder (singleton,run)
import Control.Monad.ST (runST)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?))
import Data.Semigroup (stimes)
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ THU.testCase "A" $ "ABCDEF" @=?
    ( F.toList $ run
      (  singleton 'A'
      <> singleton 'B'
      <> singleton 'C'
      <> singleton 'D'
      <> singleton 'E'
      <> singleton 'F'
      )
    )
  , THU.testCase "B" $ "ABCCCCCCCCCCCCCCCD" @=?
    ( F.toList $ run
      (  singleton 'A'
      <> singleton 'B'
      <> stimes (15 :: Int) (singleton 'C')
      <> singleton 'D'
      )
    )
  , THU.testCase "C" $ (L.replicate 500 'X') @=?
    (F.toList $ run (stimes (500 :: Int) (singleton 'X')))
  ]
