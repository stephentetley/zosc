{-# OPTIONS -Wall #-}

module Demo01 where

import ZOsc.Blob
import ZOsc.Universe
import qualified ZOsc.Decode as D
import ZOsc.Encode
import ZOsc.TimeTag

import Network.Socket
import Network.BSD

import Data.ByteString.Builder
import Data.Char
import Data.Monoid
import Data.List (foldl')
import System.IO (stdout)


demo01 = encode $ string "hello"

-- = "/test..."
test_msg = [47,116,101,115,116,0,0,0,44,105,105,105,0,0,0,0,0,0,0,1,0,0,0,2,0,0,0,3]



temp01 = map chr test_msg

test02 = encode $ typeTagString ['i', 'f', 's', 'b']

-- can use floatDec for float32

-- names in OSC are ascii (0-127)


-- newtype Remote a = Remote { 