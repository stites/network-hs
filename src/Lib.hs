--------------------------------------------------------------------------------
-- |
-- Module: Lib
-- Maintainer: Sam Stites <sam@stites.io>
--
-- This contains all nessecary interfaces to the Network.Socket module.
-- Network.Socket is a low-level module which essentially exposes the entire C
-- socket API, so there will be a lot of documentation to ensure that every facet
-- is understood.
--
-- For this reason, this library will be intentionally flat with only a few
-- functions exposed.
--------------------------------------------------------------------------------
module Lib where

import Network.Socket
import System.IO

makeSocket :: IO ()
makeSocket = do
  sock <- socket AF_INET Stream 0 -- create socket
  setSocketOption sock ReuseAddr 1 -- make socket immediate resuable for debugging
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY) -- listen on 4242
  listen sock 2 -- allow a maximum of 2 outstanding connections
  mainLoop sock

-- accept one connection and handle it
mainLoop :: Socket -> IO ()
mainLoop sock = accept sock >>= runConn >> mainLoop sock

runLoop :: (Socket, SockAddr) -> IO ()
runLoop (sock, _) = send sock "Hi!\n" >> sClose sock

runConn (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "Ping!"
  hClose hdl

