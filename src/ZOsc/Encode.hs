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

import Data.ByteString.Builder
import Data.Int
import Data.Monoid
import Data.Word

import System.IO (stdout)

printEncode :: Builder -> IO ()
printEncode = hPutBuilder stdout


nullChar :: Builder
nullChar = char7 '\0'

paddedASCIIString :: String -> Builder
paddedASCIIString ss = string7 ss <> padding
  where
    padding = let i = length ss `mod` 4 
              in case i of { 1 -> nullChar <> nullChar <> nullChar
                           ; 2 -> nullChar <> nullChar
                           ; 3 -> nullChar
                           ; _ -> mempty }
              



-- | Address has to be ASCII
--
address :: String -> Builder
address = paddedASCIIString

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



double64 :: Double -> Builder
double64 = doubleDec

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

infinitum :: Builder
infinitum = mempty

midi :: Word8 -> Word8 -> Word8 -> Word8 -> Builder
midi pid status d1 d2 = word8 pid <> word8 status <> word8 d1 <> word8 d2


-- | Special case time tag
immediately :: Builder
immediately = word64BE 1

typeTagString :: [Char] -> Builder
typeTagString = paddedASCIIString . (',':)
