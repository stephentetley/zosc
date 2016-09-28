{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZOsc.Decode
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Primitive functions to decode (read) values.
--
--------------------------------------------------------------------------------

module ZOsc.Decode
  where

import Data.Binary.IEEE754              -- package: data-binary-ieee754

import Data.Attoparsec.ByteString                   -- package: attoparsec
import qualified Data.Attoparsec.ByteString as ATTO

import Data.Bits
import qualified Data.ByteString as B
import Data.Char
import Data.Int
import Data.Word

-- Design note - we need more then Binary.Get



nullChar :: Parser Char
nullChar = '\0' <$ satisfy (==0)

padding :: Int -> Parser ()
padding n = suffix (n `mod` 4)
  where
    suffix 1 = nullChar >> nullChar >> nullChar >> return ()
    suffix 2 = nullChar >> nullChar >> return ()
    suffix 3 = nullChar >> return ()
    suffix _ = return ()

paddedASCIIByteString :: Parser B.ByteString
paddedASCIIByteString = do 
    input <- ATTO.takeWhile (/= 0)
    let len = B.length input
    padding (len `mod` 4)
    return input


paddedASCIIString :: Parser String
paddedASCIIString = map chrw . B.unpack <$> paddedASCIIByteString
  where
    chrw = chr . fromIntegral

-- | Address has to be ASCII
--
address :: Parser String
address = paddedASCIIString


int32 :: Parser Int32
int32 = fromIntegral <$> word32BE

float32 :: Parser Float
float32 = wordToFloat <$> word32BE

-- | String has to be ASCII for Open Sound Control 1.0
--
-- It is padded to be a multiple of 4 bytes.
--
string :: Parser String
string = paddedASCIIString


-- | Implemented with Data.Binary.IEEE754 @wordToDouble@
--
double64 :: Parser Double
double64 = wordToDouble <$> word64BE

-- | ASCII char padded to 4 bytes.
--
char :: Parser Char
char = (chr . fromIntegral) <$> anyWord8 <* padding 3


-- | True is not a representable value - it appears in the type tag 
-- (as True not Bool) and has no argument data
-- 
-- > true = return True
--
true :: Parser Bool
true = return True


-- | See @true@
-- 
false :: Parser Bool
false = return False

nil :: Parser ()
nil = return ()

infinitum :: Parser ()
infinitum = return ()


midi :: Parser (Word8,Word8,Word8,Word8)
midi = (,,,) <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8


--- Helpers


word16BE :: Parser Word16
word16BE = w16be <$> anyWord8 <*> anyWord8

word32BE :: Parser Word32
word32BE = w32be <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8

word64BE :: Parser Word64
word64BE = w64be <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
                 <*> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8



-- | Build a Word16 (big endian).
--
w16be :: Word8 -> Word8 -> Word16
w16be a b       = (shiftL `flip` 8  $ fromIntegral a) + fromIntegral b

-- | Build a Word24 (big endian).
--
w24be :: Word8 -> Word8 -> Word8 -> Word32
w24be a b c     = (shiftL `flip` 16  $ fromIntegral a)
                + (shiftL `flip`  8  $ fromIntegral b)
                + fromIntegral c

-- | Build a Word32 (big endian).
--
w32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32be a b c d   = (shiftL `flip` 24  $ fromIntegral a)
                + (shiftL `flip` 16  $ fromIntegral b)
                + (shiftL `flip`  8  $ fromIntegral c)
                + fromIntegral d


-- | Build a Word64 (big endian).
--
w64be :: Word8 -> Word8 -> Word8 -> Word8 
      -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
w64be a b c d e f g h = 
      (shiftL `flip` 56  $ fromIntegral a)
    + (shiftL `flip` 48  $ fromIntegral b)
    + (shiftL `flip` 40  $ fromIntegral c)
    + (shiftL `flip` 32  $ fromIntegral d)
    + (shiftL `flip` 24  $ fromIntegral e)
    + (shiftL `flip` 16  $ fromIntegral f)
    + (shiftL `flip`  8  $ fromIntegral g)
    + fromIntegral h

