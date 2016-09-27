{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZOsc.Datatypes
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Datatypes.
--
--------------------------------------------------------------------------------

module ZOsc.Datatypes
  ( 
   
    buildNotes

  ) where

import qualified Data.ByteString.Lazy as L

import Data.Time
import Data.Time.Calendar.Julian
import Data.Word

-- NOte - OSC is big endian


data Packet = Message { msg_address :: String
                      , msg_arguments :: [Atom] 
                      }
            | Bundle { bdl_time_tag :: TimeTag
                     , bdl_elements :: [Packet] 
                     }
  deriving (Eq,Ord,Show)


data Atom = Int32 Int
          | AtomTime    TimeTag 
          | Float32     Float
          | String      String
          | Blob        L.ByteString
          | Double64    Double
  deriving (Eq,Ord,Show)

data TimeTag = TimeTag 
    { _seconds_since_base :: Word32
    , _fractional_part    :: Word32
    }
  deriving (Eq,Ord,Show)



typeTag :: Atom -> Char
typeTag (Int32 {})      = 'i'
typeTag (AtomTime {})   = 't'
typeTag (Float32 {})    = 'f'
typeTag (String {})     = 's'
typeTag (Blob {})       = 'b'
typeTag (Double64 {})   = 'd'

timestamp :: IO TimeTag
timestamp = fmap post getCurrentTime
  where
    post now = let (y,m,d) = toGregorian $ utctDay now 
                   secs    = floor $ toRational $ utctDayTime now                   
               in TimeTag (secs + (fromIntegral $ secondsSince1900 y m d)) 0



mjd :: Integer -> Int -> Int -> Integer
mjd y m d = yy + fromIntegral dd
  where
    (yy,dd) = toJulianYearAndDay $ fromGregorian y m d
          

-- | From the NTP faq:
--
secondsSince1900 :: Integer -> Int -> Int -> Integer
secondsSince1900 y m d = 
    let days = mjd y m d - mjd 1900 1 1 in 86400 * days
