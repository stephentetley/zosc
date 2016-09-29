{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZOsc.Encode
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Primitive functions to encode (output) values.
--
--------------------------------------------------------------------------------

module ZOsc.Encode
  where

import ZOsc.Blob
import ZOsc.TimeTag

import Data.Binary.IEEE754              -- package: data-binary-ieee754

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Builder
import Data.Int
import Data.Monoid
import Data.Word

import System.IO (stdout)

printEncode :: Builder -> IO ()
printEncode = hPutBuilder stdout

encode :: Builder -> B.ByteString
encode = Lazy.toStrict . toLazyByteString


nullChar :: Builder
nullChar = char7 '\0'

padding :: Int -> Builder
padding n = suffix (n `mod` 4)
  where
    suffix 1 = nullChar <> nullChar <> nullChar
    suffix 2 = nullChar <> nullChar
    suffix 3 = nullChar
    suffix _ =  mempty
               

-- | Adds a null terminator and padding to 4 byte boundary
--
paddedASCIIString :: String -> Builder
paddedASCIIString ss = string7 ss <> nullChar <> padding (1+ length ss)

paddedByteString :: B.ByteString -> Builder
paddedByteString ss = byteString ss <> padding (B.length ss)



-- | Address has to be ASCII
--
address :: String -> Builder
address = paddedASCIIString


--------------------------------------------------------------------------------
-- Atomic Types

-- | Added in 1.1
--
uint32 :: Word32 -> Builder
uint32 = word32BE


-- | (Obviously) This is a signed integer.
--
int32 :: Int32 -> Builder
int32 = int32BE


float32 :: Float -> Builder
float32 = floatDec

-- | String has to be ASCII for Open Sound Control 1.0
--
-- It is padded to be a multiple of 4 bytes.
--
string :: String -> Builder
string = paddedASCIIString


int64 :: Int64 -> Builder
int64 = int64BE


blob :: Blob -> Builder
blob b = word32BE (blob_size b) <> paddedByteString (blob_data b)

asBlob :: B.ByteString -> Builder
asBlob = blob . fromByteString

timeTag :: TimeTag -> Builder
timeTag t = word64BE $ fromIntegral t


-- | Implemented with Data.Binary.IEEE754 @doubleToWord@
double64 :: Double -> Builder
double64 = word64BE . doubleToWord


-- | Implemented with ByteString.Builder @doubleDec@
--
double64' :: Double -> Builder
double64' = doubleDec

-- | ASCII char padded to 4 bytes.
--
char :: Char -> Builder
char c = paddedASCIIString [c]

-- | True is not a representable value - it appears in the type tag 
-- (as True not Bool) and has no argument data
-- 
-- > true = mempty
--
true :: Builder 
true = mempty

-- | See @true@
-- 
false :: Builder
false = mempty

nil :: Builder
nil = mempty

-- | aka @impulse@
infinitum :: Builder
infinitum = mempty

impulse :: Builder
impulse = mempty


midi :: Word8 -> Word8 -> Word8 -> Word8 -> Builder
midi pid status d1 d2 = word8 pid <> word8 status <> word8 d1 <> word8 d2


-- | Special case time tag
--
immediately :: Builder
immediately = word64BE 1

typeTagString :: [Char] -> Builder
typeTagString = paddedASCIIString . (',':)

bundleTag :: Builder
bundleTag = paddedASCIIString "#bundle"


bundle :: TimeTag -> [Builder] -> Builder
bundle tag xs = mconcat $ bundleTag : timeTag tag : map bundle1 xs

bundle1 :: Builder -> Builder
bundle1 b = uint32 len <> byteString payload
  where
    payload = encode b
    len = fromIntegral $ B.length payload
                