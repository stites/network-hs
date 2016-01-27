module Lib where

import Network.Socket
import System.IO

runnCon (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "Ping!"
  hClose hdl

