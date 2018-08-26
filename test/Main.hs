{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.Array as A
import qualified Data.Binary as B
import Data.Int (Int64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.IO (hClose)
import System.IO.Temp
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Data.Array.FS as FS

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Property tests" [ check_read ]

check_read :: TestTree
check_read = testProperty "can read" prop_read


-- TODO: This takes forever, perhaps find a more efficient test
prop_read :: Property
prop_read = property $ do

    is <- forAll (Gen.list (Range.linear 1000 1000000) (Gen.int64 Range.constantBounded))
    let is_arr = A.array (0, length is - 1) (zip [0..] is)
    indices <- forAll (replicateM 1000 (Gen.int (Range.linear 0 (snd (A.bounds is_arr)))))

    test $ runResourceT $ do

      (_, f, h) <- openTempFile Nothing "array-fs"
      liftIO (hClose h)
      liftIO (B.encodeFile f is)

      -- Each int takes 8 bytes, plus there's length at the beginning
      arr <- liftIO $ FS.new f (\i -> i*8 + 8) 100 100
               (FS.readBinary (B.get :: B.Get Int64) 100)

      forM_ indices $ \idx -> do
        let !e2 = is_arr A.! idx
        !e1 <- liftIO (FS.read (fromIntegral idx) arr)
        e1 === e2
