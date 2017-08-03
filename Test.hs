module Main where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.Exit (exitFailure)

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import Test.QuickCheck.Test

import ShamirSecretSharing (createShares, combineShares)


newtype Msg = Msg [Word8] deriving Show

instance Arbitrary Msg where
  arbitrary = do
    msg <- vectorOf 64 chooseAny
    return $ Msg msg


prop_CreateCombine :: Msg -> NonZero Word8 -> NonZero Word8 -> NonZero Word8 -> Property
prop_CreateCombine (Msg msg) (NonZero n) (NonZero k) (NonZero k2) = monadicIO $ do
    let k' = min k n
    let k2' = min k2 n
    shares <- run $ createShares msg n k'
    let restored = combineShares $ take (fromInteger (toInteger k2')) shares
    assert $ if (k2' >= k') then
        (fromJust restored) == msg
    else
        restored == Nothing


main :: IO ()
main = do
    result <- quickCheckResult prop_CreateCombine
    unless (isSuccess result) exitFailure
