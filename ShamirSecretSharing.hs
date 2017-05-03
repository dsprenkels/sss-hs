{-# LANGUAGE ForeignFunctionInterface #-}

module ShamirSecretSharing ( create_shares
                           , combine_shares
                           , share_size
                           , secret_size) where

import Data.Word (Word8)
import Foreign.Marshal.Array (allocaArray, withArray, peekArray)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception
import Data.Typeable (Typeable)


share_size :: Int
share_size = 113

secret_size :: Int
secret_size = 64


foreign import ccall "sss_create_shares" sss_create_shares_c :: Ptr Word8 -> Ptr Word8 -> Word8 -> Word8 -> IO ()
foreign import ccall "sss_combine_shares" sss_combine_shares_c :: Ptr Word8 -> Ptr Word8 -> Word8 -> IO Int


data SSSException = InvalidN
                  | InvalidK
                  | InvalidBufSize
                  | InvalidShareSize
  deriving (Show, Eq, Typeable)


instance Exception SSSException


create_shares :: [Word8] -> Word8 -> Word8 -> IO [[Word8]]
create_shares buf n k
  | n == 0                    = throw InvalidN
  | k == 0 || k > n           = throw InvalidK
  | length buf /= secret_size = throw InvalidBufSize
  | otherwise = do
    ret <- allocaArray bufsize $ \out -> do
      withArray buf $ \input -> do
        sss_create_shares_c out input n k
        peekArray bufsize out
    return $ group_shares share_size ret
  where
    bufsize = (fromIntegral n) * share_size


combine_shares :: [[Word8]] -> Maybe [Word8]
combine_shares shares
  | all (\x -> length x == share_size) shares = unsafePerformIO $ do
      allocaArray secret_size $ \out -> do
        withArray (concat shares) $ \input -> do
          ret <- sss_combine_shares_c out input k
          buf <- (peekArray secret_size out)
          return $ case ret of 0 -> Just buf
                               _ -> Nothing
  | otherwise = throw InvalidBufSize
  where
    k = fromIntegral $ length shares
    bufsize = (fromIntegral k) * share_size


group_shares :: Int -> [a] -> [[a]]
group_shares _ [] = []
group_shares n l
  | n > 0 = (take n l) : (group_shares n (drop n l))
  | otherwise = throw InvalidShareSize
