{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

module ShamirSecretSharing ( ShamirSecretSharingException
                           , createShares
                           , combineShares
                           , createKeyshares
                           , combineKeyshares
                           , shareSize
                           , secretSize
                           , keyshareSize
                           , keySize) where

import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Foreign.Marshal.Array (allocaArray, withArray, peekArray)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

shareSize :: Int
shareSize = 113

secretSize :: Int
secretSize = 64

keyshareSize :: Int
keyshareSize = 33

keySize :: Int
keySize = 32

foreign import ccall "sss_create_shares" sss_create_shares_c :: Ptr Word8 -> Ptr Word8 -> Word8 -> Word8 -> IO ()
foreign import ccall "sss_combine_shares" sss_combine_shares_c :: Ptr Word8 -> Ptr Word8 -> Word8 -> IO Int
foreign import ccall "sss_create_keyshares" sss_create_keyshares_c :: Ptr Word8 -> Ptr Word8 -> Word8 -> Word8 -> IO ()
foreign import ccall "sss_combine_keyshares" sss_combine_keyshares_c :: Ptr Word8 -> Ptr Word8 -> Word8 -> IO ()

data ShamirSecretSharingException = InvalidN
                  | InvalidK
                  | InvalidBufSize
                  | InvalidShareSize
  deriving (Show, Eq, Typeable)

instance Exception ShamirSecretSharingException

createShares :: [Word8] -> Word8 -> Word8 -> IO [[Word8]]
createShares buf n k
    | n == 0                   = throw InvalidN
    | k == 0 || k > n          = throw InvalidK
    | length buf /= secretSize = throw InvalidBufSize
    | otherwise = do
        ret <- allocaArray bufsize $ \out -> do
            withArray buf $ \input -> do
                sss_create_shares_c out input n k
                peekArray bufsize out
        return $ group shareSize ret
  where
    bufsize = (fromIntegral n) * shareSize

combineShares :: [[Word8]] -> Maybe [Word8]
combineShares shares
    | all (\x -> length x == shareSize) shares = unsafePerformIO $ do
        allocaArray secretSize $ \out -> do
            withArray (concat shares) $ \input -> do
                ret <- sss_combine_shares_c out input k
                buf <- peekArray secretSize out
                return $ case ret of 0 -> Just buf
                                     _ -> Nothing
    | otherwise = throw InvalidShareSize
  where
    k = fromIntegral $ length shares

createKeyshares :: [Word8] -> Word8 -> Word8 -> IO [[Word8]]
createKeyshares key n k
    | n == 0                = throw InvalidN
    | k == 0 || k > n       = throw InvalidK
    | length key /= keySize = throw InvalidBufSize
    | otherwise = do
        ret <- allocaArray bufsize $ \out -> do
            withArray key $ \input -> do
                sss_create_keyshares_c out input n k
                peekArray bufsize out
        return $ group keyshareSize ret
  where
    bufsize = (fromIntegral n) * keyshareSize

combineKeyshares :: [[Word8]] -> [Word8]
combineKeyshares keyshares
    | all (\x -> length x == keyshareSize) keyshares = unsafePerformIO $ do
        allocaArray keySize $ \out -> do
            withArray (concat keyshares) $ \input -> do
                sss_combine_keyshares_c out input k
                peekArray keySize out
    | otherwise = throw InvalidShareSize
  where
    k = fromIntegral $ length keyshares

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
    | n > 0 = (take n l) : (group n (drop n l))
    | otherwise = throw InvalidShareSize
