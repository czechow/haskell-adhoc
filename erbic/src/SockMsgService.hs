module SockMsgService where


import Network.Socket
import Control.Concurrent
import Control.Exception
import qualified Data.Set as S
import Control.Concurrent.BoundedChan

import Sock
import SockMsg


type Host = String

-- Main service (aka factory method)
runSockMsgService :: BoundedChan String ->
                     IO (MVar (Maybe ThreadId), MVar (S.Set ThreadId))
runSockMsgService ch = do
  mvThreads <- newMVar S.empty
  mvTid <- newMVar Nothing
  _ <- forkFinally (do modifyMVar_ mvTid $ \_ -> Just <$> myThreadId
                       srvSockMsgs "127.0.0.1" 2222 mvThreads ch)
                   (\_ -> modifyMVar_ mvTid $ \_ -> return Nothing)
  return (mvTid, mvThreads)


srvSockMsgs :: Host -> PortNumber -> MVar (S.Set ThreadId) -> BoundedChan String
            -> IO ()
srvSockMsgs host port mvThreads ch =
  bracket (ssOpen host port :: IO Socket)
          ssClose
          loop
  where
    loop s = do srvSockConn mvThreads s ch
                loop s


srvSockConn :: (SockService a, ChannelService b) =>
               MVar (S.Set ThreadId) -> a -> b -> IO ()
srvSockConn mvThreads s ch =
  mask $ \restoreMask ->
    do putStrLn $ "srvSockConn: Accepting requests"
       (s', _) <- ssAccept s
       tid <- forkFinally (thrBody s' restoreMask)
                          (thrHandler s')
       putStrLn $ "srvSockConn: new connection served by thread " ++ show tid
         where
           thrBody s'' restoreMask'' = do
             uninterruptibleMask_ $
               modifyMVar_ mvThreads
                           (\ts -> flip S.insert ts <$> myThreadId)
             restoreMask'' $ orchMsgReception s'' ch
           thrHandler s'' = \_ -> do
             ssClose s''
             uninterruptibleMask_ $
               modifyMVar_ mvThreads
                           (\ts -> flip S.delete ts <$> myThreadId)
