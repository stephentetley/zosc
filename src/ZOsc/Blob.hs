{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZOsc.Blob
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Blob datatype.
--
--------------------------------------------------------------------------------

module ZOsc.Blob
  (
    Blob(..)
  , fromByteString
  ) where


import qualified Data.ByteString as B
import Data.Word

data Blob = Blob
    { blob_size     :: !Word32
    , blob_data     :: !B.ByteString
    }
  deriving (Eq,Show)


fromByteString :: B.ByteString -> Blob
fromByteString bs = Blob { blob_size = fromIntegral $ B.length bs, blob_data = bs }