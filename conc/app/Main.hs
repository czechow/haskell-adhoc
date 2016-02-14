module Main where

import Control.Concurrent
import Control.Monad
import System.IO

import qualified Data.Map as M

main :: IO ()
main = main5

-------------------------------------------------------------jamming----------

main1 :: IO ()
main1 = do
  hSetBuffering stdout NoBuffering
  putStrLn "Up and running"
  _ <- forkIO $ job1 100
  jobMain 100
  putStrLn "Finished"


job1 :: Int -> IO ()
job1 n = foldr (>>) (return ()) $ replicate n (putChar 'J')

jobMain :: Int -> IO ()
jobMain n = replicateM_ n $ putChar 'M'

-------------------------------------------------------------------------------

main2 :: IO ()
main2 = do
  putStrLn "Enter reminder sec"
  s <- getLine
  if s == "exit" then return ()
    else do let t = read s :: Int
            putStrLn ("Ok, I'll remind you in " ++ show t ++ " seconds")
            _ <- forkIO (reminder t)
            main2

reminder :: Int -> IO ()
reminder t = do
  threadDelay ((10 :: Int)^(6 :: Int) * t)
  putStrLn (show t ++ " seconds is up! BING!\BEL")

-------------------------------------------------------------------------------

main3 :: IO ()
main3 = do
  mv <- newEmptyMVar
  putStrLn "MVar created"
  _ <- forkIO $ (mvarWriter mv)
  putStrLn "Reading from MVar..."
  rd <-takeMVar mv
  putStrLn ("Read from mvar: " ++ show rd)
  rd2 <-takeMVar mv
  putStrLn ("Read from mvar: " ++ show rd2)
  rd3 <-takeMVar mv
  putStrLn ("Read from mvar: " ++ show rd3)

mvarWriter :: MVar Int -> IO ()
mvarWriter mv = do
  threadDelay (1000*2000)
  putMVar mv 997
  threadDelay (1000*2000)
  putMVar mv 1313

-------------------------------------------------------------------------------

data Logger = Logger (MVar LogCommand)

data LogCommand = Msg String
                | Stop (MVar ())


{-
data MVar a  -- abstract

newEmptyMVar :: IO (MVar a)
newMVar      :: a -> IO (MVar a)
takeMVar     :: MVar a -> IO a
putMVar      :: MVar a -> a -> IO ()
-}

-- This should encapsulate running another thread I guess...
initLogger :: IO Logger
initLogger = do
  mv <- newEmptyMVar
  _ <- forkIO $ logLoop mv
  return $ Logger mv
  where
    logLoop mv' = do
      v <- takeMVar mv'
      case v of
        Msg s -> do
          putStrLn ("Received: [" ++ s ++ "]")
          threadDelay (1000 * 1000 * 2)
          putStrLn $ "LOG: " ++ s
          logLoop mv'
        Stop stopMV -> do
          threadDelay (1000 * 1000 * 2) -- aka flush the buffer
          putMVar stopMV ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger mv) msg = do
  putStrLn $ "LogMessage: " ++ msg
  putMVar mv $ Msg msg

logStop :: Logger -> IO ()
logStop (Logger mv) = do
  putStrLn "Stopping logger"
  stopMVar <- newEmptyMVar
  putMVar mv (Stop stopMVar)
  _ <- takeMVar stopMVar
  putStrLn "Logger stopped"
  return ()

main4:: IO ()
main4 = undefined

---------------------------------------------------------------------------

type Name = String
type PhoneNumber = String
type PhoneBook = M.Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
  mv <- newMVar M.empty
  return $ PhoneBookState mv

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState mv) name phone = do
  pb <- takeMVar mv
  let pb' = M.insert name phone pb
  putMVar mv pb'
  seq pb' (return ()) -- FIXME: will this evaluate

lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState mv) name = do
  pb <- takeMVar mv
  putMVar mv pb
  return $ M.lookup name pb

main5 :: IO ()
main5 = do
  pb <- new
  sequence_ [insert pb ("name" ++ show x) (show x) | x <- [(1::Int)..100]]
  Main.lookup pb "name83" >>= (putStrLn . show)
  Main.lookup pb "unknown" >>= (putStrLn . show)
