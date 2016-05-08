module Main where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- http://hackage.haskell.org/package/binary-0.8.3.0/docs/Data-Binary-Get.html
import Data.Binary.Get

import Data.Word

-- main :: IO ()
-- main = do
--   contents <- BL.getContents
--   BL.putStr contents

deserialiseHeader :: Get (Word32, Word32, Word32)
deserialiseHeader = do
  alen <- getWord32be
  plen <- getWord32be
  chksum <- getWord32be
  return (alen, plen, chksum)

main :: IO ()
main = do
  input <- BL.getContents
  print $ runGet deserialiseHeader input  
