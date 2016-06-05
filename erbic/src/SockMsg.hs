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


-- Error|Warn|Result
data SockMsg = SockMsg String
             | Close


recvMsg :: NS.Socket -> String -> MVar () -> IO (SockMsg, String)
recvMsg s buff mv = do
  let (msg, buff', infos) = readData buff []
  log infos
  case (msg, buff') of
    (Msg m, buff'') -> return (SockMsg m, buff'')
    (MsgPart mp, _) -> doRecv mp
    (NoMsg, _) -> doRecv []
  where
    doRecv mp  = do
      res <- recvLen s 16 mv
      case res of
        Data (d, _) -> recvMsg s (mp ++ d) mv
        _ -> return (Close, []) -- FIXME: other states too, any buff info?


log :: [String] -> IO ()
log = mapM_ putStrLn

data Msg = Msg String
         | MsgPart String
         | NoMsg
         deriving (Show)

-- FIXME: The following looks like a classical parser's job...
-- FIXME: State & Writer monad here...
readData :: String -> [String] -> (Msg, String, [String])
readData [] infos = (NoMsg, [], infos)
readData buff' infos =
  case break (=='\n') buff' of
    (msg, ('\n' : newBuff')) -> case sane msg of
      Just m -> (Msg m, newBuff', infos)
      Nothing -> readData newBuff'
                 (infos ++ ["Msg too long: " ++ show (length msg)])
    (_, _) -> case sane buff' of
      Just m -> (MsgPart m, [], infos)
      Nothing -> (NoMsg, [],
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
