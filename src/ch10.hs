{-# LANGUAGE OverloadedStrings #-}
-- http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64


import Data.Char (isSpace)

import qualified Data.ByteString.Internal as BI (c2w, w2c)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m


parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 = undefined

bytes = B64.decode "SSdtIGEgYmFzZTY0IGVuY29kZWQgQnl0ZVN0cmluZw=="
main = print bytes

encode_bytes_64 = B64.encode "I'm a base64 encoded ByteString"
encode_bytes_16 = B16.encode "I'm a base16 encoded ByteString"

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
                 | num <= 0    -> Nothing
                 | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

-- test
-- bytestring_a = L.pack "asdf"
bs_a = L8.pack "I'm a ByteString, not a [Char]"
bs_a_Char = L8.unpack bs_a      -- show as char
bs_a_Word8 = L.unpack bs_a      -- show as pure ascii code
