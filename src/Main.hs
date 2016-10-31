module Main where

import Control.Monad (unless, void)
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import qualified Network.Socket as Net
import Data.List (isInfixOf)
import System.Exit (exitSuccess)
import System.Environment (getArgs)

initServer :: String -> String -> IO Net.Socket
initServer host port = do
  let hints = Net.defaultHints { Net.addrSocketType = Net.Stream }
  addr:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port)
  print addr
  sock <- Net.socket (Net.addrFamily addr) (Net.addrSocketType addr) (Net.addrProtocol addr)
  Net.bind sock (Net.addrAddress addr)
  return sock

getDetails :: String -> [String] -> String
getDetails msg inf = msg ++ "IP:" ++ head inf ++ "\nPort:" ++ inf !! 1 ++ "\nStudentID:" ++ last inf ++ "\n"


action :: Net.Socket -> String -> [String] -> IO ()
action s msg inf | "HELO" `isInfixOf` msg = void  $ send s (pack $ getDetails msg inf)
                 | "KILL_SERVICE\n" `isInfixOf` msg = exitSuccess
                 | otherwise = void $  send s $ pack msg

handle :: Net.Socket -> [String] -> IO ()
handle sock inf = do
    msg <- Network.Socket.ByteString.recv sock 4096
    print $ "Message = " ++ unpack msg
    unless (B.null msg) $ action sock (unpack msg) inf >> handle sock inf

runServer :: Net.Socket -> Int -> [String]-> IO ()
runServer sock n inf = do
    Net.listen sock n
    (conn,_) <- Net.accept sock
    handle conn inf
    Net.close conn

main :: IO ()
main = do
  (n:port:host:_) <- getArgs
  sock <- initServer host port
  runServer sock (read n :: Int) [host, port, "13319506"]
  putStrLn "hello world"
