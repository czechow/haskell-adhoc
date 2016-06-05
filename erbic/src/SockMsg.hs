module SockMsg where

import Prelude hiding (log)
import qualified Network.Socket as NS
import Control.Concurrent

import Control.Concurrent
import System.IO
import System.Timeout
import Control.Monad
import Control.Exception


type Length = Int
data SockResult = Error String
                | Closed
                | Interrupted
                | Data (String, Int)
                deriving (Show)
type ErrMsg = String

recvLen :: NS.Socket -> Length -> MVar () -> IO SockResult
recvLen s l mv = do
  errOrData <- try' (timeout 1000000 $ NS.recvLen s l)
  case errOrData of -- can we somehow detect closing condition???
    Left e -> return $ Error $ show e
    Right maybeData -> tryReadMVar mv >>= shouldStop maybeData
  where
    shouldStop _ (Just _) = return $ Interrupted
    shouldStop md Nothing = case md of
      Just d -> return $ Data d
      Nothing -> recvLen s l mv

data ParseState = InPacket
                | NoPacket
                deriving (Show)

-- Error|Warn|Result
data SockMsg = SockMsg String
             | Close


recvMsg :: NS.Socket -> String -> ParseState -> MVar () -> IO (SockMsg, String)
recvMsg s buff ps mv = do
  res <- recvLen s 16 mv
  case res of
    Data (d, _) -> do
      let (mMsg, buff', infos) = readData (buff ++ d) ps []
      log infos
      case (mMsg, buff') of
        (Just msg, buff'') -> return (SockMsg msg, buff'')
        (Nothing, [])  -> recvMsg s [] NoPacket mv
        (Nothing, buff'')  -> recvMsg s buff'' InPacket mv
    _ -> return (Close, buff) -- FIXME: other states too

log :: [String] -> IO ()
log = mapM_ putStrLn

-- FIXME: The following looks like a classical parser's job...
-- FIXME: State & Writer monad here...
readData :: String -> ParseState -> [String] -> (Maybe String, String, [String])
readData [] NoPacket infos = (Nothing, [], infos)
readData buff' NoPacket infos = case break (=='\n') buff' of
  (dropped, ('\n' : newBuff')) ->
    readData newBuff' InPacket (infos ++ ["Dropped " ++ show (length dropped)])
  (dropped, newBuff') ->
    readData newBuff' NoPacket (infos ++ ["Dropped " ++ show (length dropped)])
readData buff' InPacket infos =
  case break (=='\n') buff' of
    (msg, ('\n' : newBuff')) -> case sane msg of
      Just m -> (Just m, newBuff', infos)
      Nothing -> readData newBuff' InPacket
                 (infos ++ ["Msg too long: " ++ show (length msg)])
    (_, _) -> case sane buff' of
      Just m -> (Nothing, m, infos)
      Nothing -> (Nothing, [],
                  (infos ++ ["Buffer too long: " ++ show (length buff')]))
  where
    sane m' = if length m' <= 20
              then Just m'
              else Nothing


try' :: IO a -> IO (Either SomeException a)
try' op = do
  res <- try op
  case res of
    Left e -> do
      putStrLn $ "***Exception encountered: " ++ show e
      return $ Left e
    _ -> return res
