module Erbic.In.SockService.Sock where


import Control.Exception
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 (unpack)
import GHC.IO.Exception


class SockService a where
  ssOpen :: String -> PortNumber -> IO a
  ssClose :: a -> IO ()
  ssRead :: a -> Int -> IO ReadRes
  ssAccept :: a -> IO (a, String)

data ReadRes = RRData String
             | RRClosed
             | RRError (Maybe Int) ErrMsg
             deriving Show

type ErrMsg = String


instance SockService Socket where
  ssOpen host port =
      bracketOnError
        (do s <- socket AF_INET Stream defaultProtocol
            putStrLn $ "Socket opened: " ++ show s -- debug
            return s)
        (\s -> ssClose s)
        (\s -> do setSocketOption s ReuseAddr 1
                  hostAddr <- inet_addr host
                  bind s (SockAddrInet port hostAddr)
                  listen s 5
                  return s)


  ssClose s = do close s
                 putStrLn $ "Socket closed: " ++ show s -- debug

  ssRead s len = (RRData . unpack <$> recv s len) `catch` handler
    where
      handler :: IOException -> IO ReadRes
      handler e = case ioe_type e of
        EOF -> return RRClosed
        _ -> return $ RRError (fromInteger . toInteger <$> ioe_errno e) $ show e

  ssAccept s = mapSnd show <$> accept s


mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)
