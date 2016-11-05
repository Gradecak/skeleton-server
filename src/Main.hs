module Main where

import Control.Monad (unless, void )
import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newMVar, newEmptyMVar)
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
  addr:_ <- Net.getAddrInfo Nothing (Just host) (Just port)
  sock <- Net.socket (Net.addrFamily addr) Net.Stream  Net.defaultProtocol-- (Net.addrProtocol addr)
  Net.bind sock (Net.addrAddress addr)
  return sock

{- A set of actions that the server will perform on recieveing keyword string -}
action :: Net.Socket -> String -> String -> MVar () -> IO ()
action s msg inf kill | "HELO" `isInfixOf` msg   = void  $ send s (pack $ msg++inf) -- send back info
                      | "KILL_SERVICE\n" == msg  = putMVar kill ()                     --terminate thread
                      | otherwise                = return ()                        -- do nothing

{- higher order function for preforming an operation on a the contents
   of a mutex variable while maintaining atomicity -}
updateMutex :: MVar a -> (a -> a) -> IO ()
updateMutex mv op = do
  x <- takeMVar mv
  putMVar mv (op x)

{- Handle an incoming connection, runs on a new thread to allow multiple
  simultaneous server connections -}
handleIncoming :: Net.Socket -> String -> MVar () -> IO ()
handleIncoming sock inf kill= do
  msg <- recv sock 4096
  print $ "Message = " ++ unpack msg
  unless (B.null msg) $ action sock (unpack msg) inf kill >> handleIncoming sock inf kill

{- Handles the termination of connection handling threads
   Closes socket and updates available threads count before -}
endThread :: Net.Socket -> MVar Int -> IO ()
endThread s m = Net.close s >> updateMutex m (+1)

{-
 Main logic of the Server
 @params:
    Socket on which the server should accept connections
    Number of connections it should process at a time
    list of details regarding the server -}
runServer :: Net.Socket -> Int -> String -> MVar () -> IO ()
runServer sock n inf kill = do
  Net.listen sock 3                    -- allow maximum of 3 queued connections
  threads <- newMVar n                 -- create a new mutex variable for counting amount of active threads
  loop threads
  where loop mu =  do
          (conn,_) <- Net.accept sock  -- accept incoming connection
          x <- takeMVar mu             -- take value from mutex
          putMVar mu x                 -- return mutex lock
          if x == 0                    -- if no free threads available
            then Net.close conn >> loop mu
            else do
            updateMutex mu (\z -> z-1) -- reduce available thread count
            _ <- forkFinally (handleIncoming conn inf kill ) (\_ -> endThread conn mu ) -- handle incoming connection on a new thread
                                                                                  -- when the thread terminates call 'endThread'
            loop mu

main :: IO ()
main = do
  [host, port, n] <- getArgs
  sock <- initSocket host port -- intialise the server socket
  putStrLn $ "staring server on " ++ host ++ ":" ++ port
  kill <- newEmptyMVar
  _ <- forkIO $ runServer sock  (read n :: Int) ("IP:10.62.0.104\nPort:"++port++"\nStudentID:13319506\n") kill
  takeMVar kill
  putStrLn "Terminating Server"
