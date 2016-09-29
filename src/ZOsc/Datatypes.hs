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
-- Datatypes (out-of-date).
--
--------------------------------------------------------------------------------

module ZOsc.Datatypes
  where

import ZOsc.TimeTag


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



typeTag :: Atom -> Char
typeTag (Int32 {})      = 'i'
typeTag (AtomTime {})   = 't'
typeTag (Float32 {})    = 'f'
typeTag (String {})     = 's'
typeTag (Blob {})       = 'b'
typeTag (Double64 {})   = 'd'

