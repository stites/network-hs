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
  sock <- chatSocket -- pull out a pure socket
  setChatSocketOptions sock
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY) -- listen on 4242
  listen sock 2 -- allow a maximum of 2 outstanding connections
  mainLoop sock

-- | This creates a @Stream@ socket using the AF_INET address space with a
-- protocol number of 0. Here is the detailed description of each component:
--
--  + socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
--
--  + AF_INET: the address family. The Linux kernel supports 29 other families
--    including UNIX (AF_UNIX), IPX (AF_IPX), communications with Bluetooth, IRDA.
--    AF_INET is for socket communications over a network and is the most generic
--    option for use. To use IPv6, check out AF_INET6. Usual values are AF_INET,
--    AF_INET6, or AF_UNIX. if the AF_INET6 address space is used, then the
--    @IPv6Only@ option is set to 0 so that both IPv6 and IPv6 can be handled on
--    one socket.
--
--  + Stream: the socket type. Usually either Stream, which translates to
--    SOCK_STREAM, or Datagram, which translates to SOCK_DGRAM. A stream socket is
--    like a phone call, a datagram socket is like passing notes in class. See:
--    http://stackoverflow.com/a/4688899/1529734
--
--  + defaultProtocol: The protocol number this can be set to a specific value, but
--    @defaultProtocol@ will flexibly accomodate the address space.
--
chatSocket :: IO Socket
chatSocket = socket AF_INET Stream defaultProtocol

-- | Configure a socket with all nessecary options here. Options used:
--
--  + ReuseAddr (SO_REUSEADDR) - Control whether @bind@ should permit reuse of
--    local addresses for this socket. This allows us to actually have two
--    sockets with the same IP port. Using two identially-named sockets would
--    confuse the Internet and the system won't let you do so. This is usually
--    for higher-level internet protocols, like FTP, which require this. We are
--    just going to use this for debugging. Use a nonzero value to turn on.
--
-- See https://www.gnu.org/software/libc/manual/html_node/Socket-Options.html
-- for more details.
--
setChatSocketOptions :: Socket -> IO ()
setChatSocketOptions sock = do
  setSocketOption sock ReuseAddr 1

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

