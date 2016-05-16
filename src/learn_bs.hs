{-# LANGUAGE OverloadedStrings #-}

module LearnBs where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- http://hackage.haskell.org/package/binary-0.8.3.0/docs/Data-Binary-Get.html
import Data.Binary.Get

import Data.Word


import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64



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


bytes = B64.decode "SSdtIGEgYmFzZTY0IGVuY29kZWQgQnl0ZVN0cmluZw=="

encode_bytes_64 = B64.encode "I'm a base64 encoded ByteString"
encode_bytes_16 = B16.encode "I'm a base16 encoded ByteString"


-- test
-- bytestring_a = L.pack "asdf"
bs_a = BL8.pack "I'm a ByteString, not a [Char]"
bs_a_Char = BL8.unpack bs_a      -- show as char
bs_a_Word8 = BL.unpack bs_a      -- show as pure ascii code


