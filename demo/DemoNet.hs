{-# OPTIONS -Wall #-}

module DemoNet where

import qualified ZOsc.Decode as Dec
import qualified ZOsc.Encode as Enc
import ZOsc.Universe

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD

import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Char
import Data.Monoid
import Data.List (foldl')
import System.IO (stdout)


-- = "/test..."
test_msg = B.pack [47,116,101,115,116,0,0,0,44,105,105,105,0,0,0,0,0,0,0,1,0,0,0,2,0,0,0,3]

test_pkt = Message "/test" [ Int32 1 ]

main = main2 "127.0.0.1" 9001
		 
main2 :: String -> PortNumber -> IO ()
main2 hostname portnum = withSocketsDo $ do 
    sckt <- socket AF_INET Datagram 0
    server <- inet_addr hostname
    let sa = SockAddrInet portnum server
    sendTo sckt test_msg sa 
    sClose sckt

oneshotSend :: String -> PortNumber -> Packet -> IO ()
oneshotSend hostname portnum pkt = withSocketsDo $ do 
    sckt <- socket AF_INET Datagram 0
    server <- inet_addr hostname
    let sa = SockAddrInet portnum server
    sendTo sckt (encode pkt)  sa 
    sClose sckt

-- fails...
demo01 = decode $ encode $ test_pkt


demo02 = Dec.decode Dec.address (Enc.encode $ Enc.address "/home")

demo02a = Enc.encode $ Enc.address "/home"
demo02b = Dec.decode Dec.address (B.pack $ map (fromIntegral . ord) $ "/home\0\0\0")

temp01 = map (chr . fromIntegral) $ B.unpack test_msg

test02 = hPutBuilder stdout $ Enc.typeTagString ['i', 'f', 's', 'b']



-- can use floatDec for float32

-- names in OSC are ascii (0-127)


-- newtype Remote a = Remote { 