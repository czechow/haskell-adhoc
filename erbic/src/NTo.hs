{-# LANGUAGE LambdaCase #-}

module NTo where


import System.Timeout
import System.IO
import Control.Concurrent
import Control.Exception
import Network.Socket
import Control.Monad
import GHC.IO.Exception
import Data.List.Split (splitOn)
import Data.IORef
import System.Random
import GHC.IO (unsafeUnmask)
import qualified Data.Set as S
import Data.List (isInfixOf)
import Test.QuickCheck


type ErrMsg = String

-- This may throw an exception
openSock :: IO Socket
openSock =
      bracketOnError
        (socket AF_INET Stream defaultProtocol)
        (\s -> do close s
                  putStrLn $ "openSock: Socket closed " ++ show s)
        (\s -> do
            setSocketOption s ReuseAddr 1
            hostAddr <- inet_addr "127.0.0.1"
            putStrLn $ "Acquired socket " ++ show s
            bind s (SockAddrInet 2222 hostAddr)
            listen s 5
            threadDelay 5000000
            return s)


data SockReadRes = SRRData String
                 | SRRClosed
                 | SRRErr (Maybe Int) ErrMsg
                 deriving Show


-- readSock :: Socket -> IO SockReadRes
-- readSock s = readWith (recv s 8)


-- readWith :: IO String -> IO SockReadRes
-- readWith readFun = recv' `catch` handler
--   where
--     recv' = liftM SRRData $ readFun
--     handler :: IOException -> IO SockReadRes
--     handler e = do
--       putStrLn $ "Running e handler"
--       let maybeErrCode = fromInteger . toInteger <$> ioe_errno e
--       putStrLn $ "Exception is [" ++ show maybeErrCode ++ "][" ++ show e ++ "]"
--       case (ioe_type e) of
--         EOF -> return SRRClosed
--         _ -> return $ SRRErr maybeErrCode (show e)

srvConn :: MVar (S.Set ThreadId) -> Socket -> IO ()
srvConn mvThreads s =
  mask $ \restoreMask ->
    do putStrLn $ "srvConn: Accepting requests"
       (s', _) <- accept s
       tid <- forkFinally (thrBody s' restoreMask)
                          (thrHandler s')
       putStrLn $ "srvConn: new connection served by thread " ++ show tid
         where
           thrBody s'' restoreMask'' = do
             uninterruptibleMask_ $
               modifyMVar_ mvThreads
                           (\ts -> flip S.insert ts <$> myThreadId)
             restoreMask'' $ doServeConn s''
           thrHandler s'' = \_ -> do
             close s''
             uninterruptibleMask_ $
               modifyMVar_ mvThreads
                           (\ts -> flip S.delete ts <$> myThreadId)
             putStrLn $ "Sock closed " ++ show s''

nset :: IO (MVar (S.Set ThreadId))
nset = newMVar S.empty


doServeConn :: Socket -> IO ()
doServeConn s = do
  ms <- getMaskingState
  putStrLn $ "doServerConn: Mask state is " ++  show ms
  recv s 16 >>= putStrLn
  doServeConn s


doAll :: MVar (S.Set ThreadId) -> Int -> IO ()
doAll mv n = bracket
             (openSock)
             (\s -> do close s
                       putStrLn $ "doAll: Closed socket " ++ show s)
             (\s -> loop n s)
  where
    loop :: Int -> Socket -> IO ()
    loop n' s
      | n' <= 0 = return ()
      | otherwise = do _ <- srvConn mv s
                       loop (pred n') s


mvInt :: IO (MVar Int)
mvInt = newMVar 0


conc :: MVar Int -> IO ()
conc mv = do
  modifyMVar_ mv $ return . const 0
  conc mv


isCloseIntr :: MVar Int -> IO ()
isCloseIntr mv = do
  s <- openSock
  putStrLn $ "Repeatedly closing socket"
  mask_ $ loop s
    where
      loop s' = do close s'
                   uninterruptibleMask $ \_ -> modifyMVar_ mv $ return . succ
                   loop s'

nonIntr :: IO ()
nonIntr = do
  mask_ (do putStrLn "Sleep started"
            threadDelay $ 10 * 1000 * 1000
            putStrLn "Sleep stopped")


class SockService a where
  ssOpen :: IO a
  ssClose :: a -> IO ()
  ssRead :: a -> IO String
  ssAccept :: a -> IO (a, String)

instance SockService Socket where
  ssOpen =
      bracketOnError
        (socket AF_INET Stream defaultProtocol)
        (\s -> ssClose s)
        (\s -> do setSocketOption s ReuseAddr 1
                  hostAddr <- inet_addr "127.0.0.1"
                  bind s (SockAddrInet 2222 hostAddr)
                  listen s 5
                  return s)
  ssClose s = close s
  ssRead s = recv s 16
  ssAccept s = do (s', sockAddr) <- accept s
                  return (s', show sockAddr)

newtype MockSocket = MockSocket ()

instance SockService MockSocket where
  ssOpen = return $ MockSocket ()
  ssClose _ = return ()
  ssRead _ = do threadDelay $ 3 * 1000 * 1000
                return $ "This and that and nothing else"
  ssAccept _ = return (MockSocket(), "127.0.0.1")


newMS :: MockSocket
newMS = MockSocket ()

------------------------
srvConn2 :: SockService ss => MVar (S.Set ThreadId) -> ss -> IO ()
srvConn2 mvThreads s =
  mask $ \restoreMask ->
    do putStrLn $ "srvConn: Accepting requests"
       (s', _) <- ssAccept s
       tid <- forkFinally (thrBody s' restoreMask)
                          (thrHandler s')
       putStrLn $ "srvConn: new connection served by thread " ++ show tid
         where
           thrBody s'' restoreMask'' = do
             uninterruptibleMask_ $
               modifyMVar_ mvThreads
                           (\ts -> flip S.insert ts <$> myThreadId)
             restoreMask'' $ doServeConn2 s''
           thrHandler s'' = \_ -> do
             ssClose s''
             uninterruptibleMask_ $
               modifyMVar_ mvThreads
                           (\ts -> flip S.delete ts <$> myThreadId)


doServeConn2 :: SockService ss => ss -> IO ()
doServeConn2 s = do
  ms <- getMaskingState
  putStrLn $ "doServerConn: Mask state is " ++  show ms
  ssRead s >>= putStrLn
  doServeConn2 s



-----------------------------------------------------------------------------
type Buffer = String

data PacketParse = PPIn | PPOut
                 deriving Show

type LogMsg = String

maxBuffLen :: Int
maxBuffLen = 23

sep :: String
sep = "-"

data ParseRes = PRData [String]
              | PRNeedMoreData
              deriving Show

type Msg = String

scanForMsg' :: String -> Buffer -> PacketParse
            -> ([Msg], Buffer, PacketParse)
scanForMsg' xs buff pp' =
  let buff' = buff ++ xs
      drpCnt = length buff' - maxBuffLen
  in if drpCnt > 0
     then scanForMsg' [] (drop drpCnt buff') PPOut
     else splitMsg buff' pp'
  where
    splitMsg buff'' pp = case (splitOn sep buff'', pp) of
      (a@(_ : _ : _), PPIn) -> (init a, last a, pp)
      (a@(m : _ : _), PPOut) -> (init $ tail a, last a, PPIn)
      (m : _, PPOut) -> ([], drop (length m) buff'', PPOut)
      (_, PPIn) -> ([], buff'',  PPIn)
      ([], pp'') -> ([], [], pp'')


prop_test2 :: Int -> Bool
prop_test2 = \s -> s == s

prop_sfm1 :: String -> String -> Bool
prop_sfm1 xs buff = let (_, lo, _) = scanForMsg' xs buff PPOut
                    in not $ sep `isInfixOf` lo

newtype Input = Input String
              deriving Show


instance Arbitrary Input where
  arbitrary = Input . concat <$> listOf (oneof [elements (map (:[]) ['a'..'z']), return sep])


genInput :: Gen String
genInput = concat <$> listOf (oneof [elements (map (:[]) ['a'..'z']), return sep])

genInput2 =

prop_sfm2 :: String -> String -> Bool


main :: IO ()
main = quickCheck prop_test2
