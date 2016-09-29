{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZOsc.TimeTag
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- TimeTag datatype.
--
--------------------------------------------------------------------------------

module ZOsc.TimeTag
 where


import Data.Bits
import Data.Data
import Data.Ix
import Data.Time
import Data.Time.Calendar.Julian
import Data.Word



newtype TimeTag = TimeTag { getTimeTag :: Word64 }
  deriving (Enum,Eq,Integral,Data,Num,Ord,Read,Real,Show,Ix)

fromSeconds :: Word32 -> TimeTag
fromSeconds s = TimeTag $ sw64 `shiftL` 32
  where
    sw64 = fromIntegral s


fromSecondsAndPicos :: Word32 -> Word32 -> TimeTag
fromSecondsAndPicos s p = fromSeconds s + (TimeTag $ fromIntegral p)

immediate :: TimeTag
immediate = TimeTag 1


timeTagNow :: IO TimeTag
timeTagNow = fmap post getCurrentTime
  where
    post now = let (y,m,d) = toGregorian $ utctDay now 
                   secs    = floor $ toRational $ utctDayTime now                   
               in fromSeconds (secs + (fromIntegral $ secondsSince1900 y m d))



mjd :: Integer -> Int -> Int -> Integer
mjd y m d = yy + fromIntegral dd
  where
    (yy,dd) = toJulianYearAndDay $ fromGregorian y m d
          

-- | From the NTP faq:
--
secondsSince1900 :: Integer -> Int -> Int -> Integer
secondsSince1900 y m d = 
    let days = mjd y m d - mjd 1900 1 1 in 86400 * days
