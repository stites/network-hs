module Main where

import Network.Socket

import Lib

main :: IO ()
main = makeSocket

makeSocket :: IO ()
makeSocket = do
  sock <- socket AF_INET Stream 0 -- create socket
  setSocketOption sock ReuseAddr 1 -- make socket immediate resuable for debugging
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY) -- listen on 4242
  listen sock 2 -- allow a maximum of 2 outstanding connections
  mainLoop sock

-- accept one connection and handle it
mainLoop :: Socket -> IO ()
mainLoop sock = accept sock >>= runnCon >> mainLoop sock

runLoop :: (Socket, SockAddr) -> IO ()
runLoop (sock, _) = send sock "Hi!\n" >> sClose sock

