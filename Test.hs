module Main where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.Exit (exitFailure)

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import Test.QuickCheck.Test

import ShamirSecretSharing ( createShares
                           , combineShares
                           , createKeyshares
                           , combineKeyshares)


newtype Msg = Msg [Word8] deriving Show

instance Arbitrary Msg where
  arbitrary = do
    msg <- vectorOf 64 chooseAny
    return $ Msg msg

newtype Key = Key [Word8] deriving Show

instance Arbitrary Key where
  arbitrary = do
    msg <- vectorOf 32 chooseAny
    return $ Key msg

prop_CreateCombineShares :: Msg -> NonZero Word8 -> NonZero Word8 -> NonZero Word8 -> Property
prop_CreateCombineShares (Msg msg) (NonZero n) (NonZero k) (NonZero k') = monadicIO $ do
    pre $ k <= n
    pre $ k' <= n
    shares <- run $ createShares msg n k
    let restored = combineShares $ take (fromInteger (toInteger k')) shares
    monitor (counterexample $ "restored share: " ++ show restored)
    assert $ if (k' >= k) then
        (fromJust restored) == msg
    else
        restored == Nothing

prop_CreateCombineKeyshares :: Key -> NonZero Word8 -> NonZero Word8 -> NonZero Word8 -> Property
prop_CreateCombineKeyshares (Key key) (NonZero n) (NonZero k) (NonZero k') = monadicIO $ do
    pre $ k <= n
    pre $ k' <= n
    keyshares <- run $ createKeyshares key n k
    let restored = combineKeyshares $ take (fromInteger (toInteger k')) keyshares
    monitor (counterexample $ "restored keyshare: " ++ show restored)
    assert $ (k' >= k) == (restored == key)

main :: IO ()
main = do
    results <- sequence $ [ quickCheckResult prop_CreateCombineShares
                          , quickCheckResult prop_CreateCombineKeyshares]
    unless (all isSuccess results) exitFailure
