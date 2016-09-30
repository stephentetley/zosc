{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZOsc.Universe
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Fixed universe of datatypes.
--
--------------------------------------------------------------------------------

module ZOsc.Universe
  where


import ZOsc.Blob
import qualified ZOsc.Decode as Dec
import qualified ZOsc.Encode as Enc
import ZOsc.TimeTag

import Data.Attoparsec.ByteString                   -- package: attoparsec

import Control.Monad ( void, mplus )
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Int
import Data.Monoid

-- Note - OSC is big endian


data Packet = Message { msg_address :: String
                      , msg_arguments :: [Atom] 
                      }
            | Bundle { bdl_time_tag :: TimeTag
                     , bdl_elements :: [Packet] 
                     }
  deriving (Eq,Ord,Show)


data Atom = Int32       Int32
          | AtomTime    TimeTag 
          | Float32     Float
          | String      String
          | AtomBlob    Blob
          | Double64    Double
  deriving (Eq,Ord,Show)



typeTag :: Atom -> Char
typeTag (Int32 {})      = 'i'
typeTag (AtomTime {})   = 't'
typeTag (Float32 {})    = 'f'
typeTag (String {})     = 's'
typeTag (AtomBlob {})   = 'b'
typeTag (Double64 {})   = 'd'


--------------------------------------------------------------------------------
-- Encode

encode :: Packet -> B.ByteString
encode = Enc.encode . encode1

encode1 :: Packet -> Builder
encode1 (Message { msg_address = addr, msg_arguments = args }) =
    let tys = map typeTag args
    in Enc.address addr <> Enc.typeTagString tys <> mconcat (map encodeAtom args)

encode1 (Bundle { bdl_time_tag = tt, bdl_elements = ps }) = 
    mconcat $ Enc.bundleTag : Enc.timeTag tt : map (Enc.bundle1 . encode1) ps
                     

encodeAtom :: Atom -> Builder
encodeAtom (Int32 i)            = Enc.int32 i
encodeAtom (AtomTime tt)        = Enc.timeTag tt
encodeAtom (Float32 d)          = Enc.float32 d
encodeAtom (String s)           = Enc.string s
encodeAtom (AtomBlob b)         = Enc.blob b
encodeAtom (Double64 d)         = Enc.double64 d


--------------------------------------------------------------------------------
-- Decode

decode :: B.ByteString -> Either String Packet
decode = Dec.decode packet 

packet :: Parser Packet
packet = do 
   decodeBundle `mplus` decodeMessage

packetSz :: Parser Packet
packetSz = do 
   len <- Dec.uint32
   Dec.sized (fromIntegral len) packet


decodeBundle :: Parser Packet
decodeBundle = do 
    void $ Dec.bundleTag
    tt <- Dec.timeTag
    ps <- many1 packetSz
    return $ Bundle { bdl_time_tag = tt, bdl_elements = ps }

decodeMessage :: Parser Packet
decodeMessage = do 
    addr <- Dec.address
    tys  <- Dec.typeTagString
    args <- mapM decodeAtom tys
    return $ Message { msg_address = addr, msg_arguments = args }


decodeAtom :: Char -> Parser Atom
decodeAtom 'i'          = Int32     <$> Dec.int32
decodeAtom 't'          = AtomTime  <$> Dec.timeTag
decodeAtom 'f'          = Float32   <$> Dec.float32
decodeAtom 's'          = String    <$> Dec.string
decodeAtom 'b'          = AtomBlob  <$> Dec.blob
decodeAtom 'd'          = Double64  <$> Dec.double64
decodeAtom _            = mempty    <?> "atom failed"             
