module Main where

import Control.Monad (unless, void )
import Control.Concurrent (forkFinally, forkIO)
import Network.Info (getNetworkInterfaces, ipv4)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newMVar)
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

{- A set of actions that the server will perform on recieveing keyword string -}
action :: Net.Socket -> String -> String -> IO ()
action s msg inf | "HELO" `isInfixOf` msg   = void  $ send s (pack $ msg++ inf)
                 | "KILL_SERVICE\n" == msg  = exitSuccess
                 | otherwise                = return ()

{- Preforms an operation on a the contents of a mutex variable while maintaining atomicity -}
updateMutex :: MVar a -> (a -> a) -> IO ()
updateMutex mv op = do
  x <- takeMVar mv
  putMVar mv (op x)

{- Handle an incoming connection, ideally should be run on a new thread to allow multiple
  simultaneous server connections -}
handle :: Net.Socket -> String -> IO ()
handle sock inf = do
  msg <- recv sock 4096 -- readAll sock 1 (return []) -- read the entire message from the socket
  print $ "Message = " ++ unpack msg
  unless (B.null msg) $ action sock (unpack msg) inf >> handle sock inf
  print "Im back yo"

{- reads the entire message from the socket, this is used instead of default recv function
   as recv is not guaranteed to return the entire message -}
readAll :: Net.Socket -> Int -> IO [B.ByteString] -> IO B.ByteString 
readAll _ 0 iob = fmap B.concat iob
readAll s _ m = do
  msg <- recv s 4096
  readAll s (B.length msg) $ m >>= (\p -> return(p++[msg]))

{- Handles the termination of connection handling threads -}
endThread :: Net.Socket -> MVar Int -> IO ()
endThread s m = Net.close s >> updateMutex m (+1)

{-
 Main logic of the Server
 @params:
    Socket on which the server should accept connections
    Number of connections it should procees at a time
    list of details regarding the server -}
runServer :: Net.Socket -> Int -> String-> IO ()
runServer sock n inf = do
  Net.listen sock 3                    -- allow maximum of 3 queued connections
  threads <- newMVar n
  loop sock threads inf
  where loop s mu i =  do
          (conn,_) <- Net.accept sock  -- accept incoming connection
          x <- takeMVar mu
          putMVar mu x
          if x == 0                    -- if no free threads available
            then Net.close conn >> loop s mu i
            else do
            updateMutex mu (\z -> z-1) -- reduce available thread count
            _ <- forkFinally (handle conn inf) (\_ -> endThread conn mu )
            loop s mu i

main :: IO ()
main = do
  [host, port, n] <- getArgs
  ns <- getNetworkInterfaces
  sock <- initServer host port -- intialise the server socket
  runServer sock  (read n :: Int) $ "IP:"++show (ipv4 $ head ns)++"\nPort:"++port++"\nSudentID:13319506\n"
