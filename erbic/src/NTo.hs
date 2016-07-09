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
import Data.List (isInfixOf, intercalate)
import Control.Monad.State
import Control.Monad.Identity
import qualified Control.Monad.Writer as MW
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


nset :: IO (MVar (S.Set ThreadId))
nset = newMVar S.empty


-- doServeConn :: Socket -> IO ()
-- doServeConn s = do
--   ms <- getMaskingState
--   putStrLn $ "doServerConn: Mask state is " ++  show ms
--   recv s 16 >>= putStrLn
--   doServeConn s


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
      | otherwise = do _ <- srvConn2 mv s
                       loop (pred n') s




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
srvConn2 :: SockService a => MVar (S.Set ThreadId) -> a -> IO ()
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
doServeConn2 s =
  doServeConn2' $ mkScanData "" PPIn
  where
    doServeConn2' sd' = do
      str <- ssRead s
      let (res, sd'') = runScan (filter nonCRLF str) sd'
      putStrLn $ "Read: [" ++ intercalate "|" res ++ "]"
      -- here we should progress with destination => like channel or sth
      doServeConn2' sd''
    nonCRLF x = x /= '\n' && x /= '\r'



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


data ScanData = ScanData { leftOver :: Buffer
                         , parseState :: PacketParse
                         , overrunCnt :: Int
                         , syncCnt :: Int }
              deriving Show

mkScanData :: Buffer -> PacketParse -> ScanData
mkScanData buff pp = ScanData buff pp 0 0

scanForMsg'' :: String -> State ScanData [Msg]
scanForMsg'' xs = do
  sd@(ScanData buff pp oCnt sCnt) <- get
  let buff' = buff ++ xs
      drpCnt = length buff' - maxBuffLen
  if drpCnt > 0
    then do put sd { leftOver = (drop drpCnt buff')
                   , parseState = PPOut
                   , overrunCnt = (succ oCnt) }
            scanForMsg'' []
    else case (splitOn sep buff', pp) of
      (a@(_ : _ : _), PPIn) -> do put sd { leftOver = last a }
                                  return $ init a
      (a@(_ : _ : _), PPOut) -> do put sd { leftOver = last a
                                          , parseState = PPIn
                                          , syncCnt = succ sCnt }
                                   return $ init $ tail a
      (m : _, PPOut) -> do put sd { leftOver = drop (length m) buff' }
                           return []
      (_, _) -> do put sd { leftOver = buff' }
                   return []


runScan :: String -> ScanData -> ([Msg], ScanData)
runScan xs s = runState (scanForMsg'' xs) s



prop_1 :: Property
prop_1 = forAll genInput3 $ \(xs, buff, pp) ->
  let (res, (ScanData lo _ _ _)) = runScan xs $ mkScanData buff pp
  in (not $ sep `isInfixOf` lo)
     &&
     (not $ sep `isInfixOf` concat res)

prop_2 :: Property
prop_2 = forAll genNoOverflow $ \(xs, buff) ->
  let (rss, (ScanData lo _ _ _)) = runScan xs $ mkScanData buff PPIn
  in concat (map (++sep) rss) ++ lo == buff ++ xs

prop_3 :: Property
prop_3 =
  forAll genNoOverflow $ \(xs, buff) ->
  let (rss, (ScanData lo _ _ sc)) = runScan xs $ mkScanData buff PPOut
  in concat rss ++ lo == (concat . tail $ splitOn sep (buff ++ xs))
     &&
     sc == if sep `isInfixOf` (buff ++ xs) then 1 else 0

prop_4 :: Property
prop_4 = forAll genOverflow $ \(xs, buff, pp) ->
  let (rss, (ScanData lo _ ovr sc)) = runScan xs $ mkScanData buff pp
      input' = drop ((length $ buff ++ xs) - maxBuffLen) (buff ++ xs)
      (rss', (ScanData lo' _ _ sc')) = runScan input' $ mkScanData "" PPOut
  in ovr == 1 && sc == sc' &&
     rss == rss' && lo == lo'


{-
  Input IO => runScan => output msgs => do something with them
-}





genInput :: Gen String
genInput = concat <$> listOf (oneof [elements (map (:[]) ['a'..'z']),
                                     return sep])

genNoOverflow :: Gen (String, String)
genNoOverflow = do
  xs <- resize maxBuffLen genInput
  p <- choose (0, length xs)
  return $ splitAt p xs

genOverflow :: Gen (String, String, PacketParse)
genOverflow = do
  xs <- resize (5 * maxBuffLen) genInput
  p <- choose (0, length xs)
  pp <- elements [PPIn, PPOut]
  if length xs <= maxBuffLen
    then genOverflow
    else let (x, y) = splitAt p xs
         in return (x, y, pp)

genInput2 :: Gen (String, String)
genInput2 = (,) <$> genInput <*> elements (map (:[]) ['a'..'z'])

genInput3 :: Gen (String, String, PacketParse)
genInput3 = (,,) <$> genInput
                 <*> genInput
                 <*> elements [PPIn, PPOut]


main :: IO ()
main = do
  ts <- nset
  doAll ts 2
