module Main where

import Control.Monad (unless, void )
import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newMVar)
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import qualified Network.Socket as Net
import Data.List (isInfixOf)
import System.Exit (exitSuccess)
import System.Environment (getArgs)

{- Initialise the socket that will be used for the server-}
initSocket :: String -> String -> IO Net.Socket
initSocket host port = do
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
handleIncoming :: Net.Socket -> String -> IO ()
handleIncoming sock inf = do
  msg <- recv sock 4096 
  print $ "Message = " ++ unpack msg
  unless (B.null msg) $ action sock (unpack msg) inf >> handleIncoming sock inf

{- Handles the termination of connection handling threads -}
endThread :: Net.Socket -> MVar Int -> IO ()
endThread s m = Net.close s >> updateMutex m (+1)

{-
 Main logic of the Server
 @params:
    Socket on which the server should accept connections
    Number of connections it should process at a time
    list of details regarding the server -}
runServer :: Net.Socket -> Int -> String-> IO ()
runServer sock n inf = do
  Net.listen sock 3                    -- allow maximum of 3 queued connections
  threads <- newMVar n                 -- create a new mutex variable for counting amount of active threads
  loop threads
  where loop mu =  do
          (conn,_) <- Net.accept sock  -- accept incoming connection
          x <- takeMVar mu
          putMVar mu x
          if x == 0                    -- if no free threads available
            then Net.close conn >> loop mu
            else do
            updateMutex mu (\z -> z-1) -- reduce available thread count
            _ <- forkFinally (handleIncoming conn inf) (\_ -> endThread conn mu ) -- handle incoming connection on a new thread
                                                                                  -- when the thread terminates call 'endThread'
            loop mu

main :: IO ()
main = do
  [host, port, n] <- getArgs
  sock <- initSocket host port -- intialise the server socket
  runServer sock  (read n :: Int) $ "IP:10.62.0.104\nPort:"++port++"\nStudentID:13319506\n"
