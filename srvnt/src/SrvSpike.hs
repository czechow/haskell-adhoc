{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SrvSpike where


import GHC.Generics
import Servant -- perhaphs qualified
import Servant.Server.Experimental.Auth
import Data.Aeson (ToJSON, FromJSON)
import qualified Network.Wai.Handler.Warp as Warp
--import qualified Network.Wai.Handler.WarpTLS as Warp
import Network.Wai (Request, requestHeaders)
import Control.Monad.Except
--import Data.Text (Text)
import Data.IORef
import qualified Data.Map.Strict as M
import Control.Concurrent (threadDelay)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.ByteString.Char8 (unpack)

type UserName = String

data User = User { name :: UserName
                 , age :: Int
                 }
          deriving (Eq, Show, Generic)

data Login = Login { login :: UserName
                   , passwd :: String
                   }
           deriving (Eq, Show, Generic)

instance FromJSON Login


type PrivateApi =
       "test" :> Get '[JSON] User
  :<|> "test2" :> Get '[JSON] User


type UserApi =
       "login" :> ReqBody '[JSON] Login
               :> Get '[JSON] (Headers '[Header "Set-Cookie" String] User)
  :<|> "private" :> AuthProtect "cookie-auth" :> PrivateApi
    -- (
    --        "test" :> Get '[JSON] User
    --   :<|> "test2" :> Get '[JSON] User
    -- )
--  :<|> "users" :> Get '[JSON] [User]

users :: [User]
users = [ User "Paul" 34
        , User "Johny" 45
        ]

instance ToJSON User

-- 'Boilerplate' ;-) here
userAPI :: Proxy UserApi
userAPI = Proxy

-- Authentication
type instance AuthServerData (AuthProtect "cookie-auth") = (UserName, CookieVal)

-- app :: Application
-- app = serve userAPI server

-- go :: IO ()
-- go = Warp.run 2222 app

type CookieVal = String
type CookieUserMap = M.Map CookieVal UserName

go :: IO ()
go = do
  ior <- newIORef (M.empty :: CookieUserMap)
  Warp.run 2222 $ serveWithContext userAPI (genAuthContext ior) $ server ior

-- gotls :: IO ()
-- gotls =
--   Warp.runTLS
--     (Warp.tlsSettings "server.crt" "server.key")
--     (Warp.setPort 2222 Warp.defaultSettings)
--     (serveWithContext userAPI (genAuthServerContext 23) genAuthServer)

go2 :: IO ()
go2 = undefined -- should launch two listeners ;-)

genAuthContext :: IORef CookieUserMap
               -> Context (AuthHandler Request (UserName, CookieVal) ': '[])
genAuthContext ior = authHandler ior :. EmptyContext

-- get-cookie
-- from the cookie we should be able to deduce the user
authHandler :: IORef CookieUserMap -> AuthHandler Request (UserName, CookieVal)
authHandler ior = mkAuthHandler handler
  where
    handler req = do
      liftIO $ do putStrLn $ "Request begin ---------"
                  putStrLn $ show req
                  putStrLn $ "Request end ---------"
      case lookup "Cookie" (requestHeaders req) >>= parseCookie . unpack of
        Just c -> do
          liftIO $ putStrLn $ "Detected cookie value: [" ++ c ++ "]"
          m <- liftIO $ readIORef ior
          case M.lookup c m of
            Just u -> return (u, c)
            Nothing -> throwError (err403 { errBody = "User not logged" })
        Nothing -> throwError (err403 { errBody = "[Perm denied]" })

server :: IORef CookieUserMap -> Server UserApi
server ior =
       login'
  :<|> private
  -- :<|> logout'
  where -- check users from configuration here
    login' (Login u@"neo" "knock") = do
      _ <- liftIO $
           atomicModifyIORef' ior $ \m -> (M.insert (mkToken u) u m, ())
      return $ addHeader (mkCookie $ mkToken u) $ head users
    login' _ = throwError (err403 { errBody = "Permission denied" }) -- or redir

type Token = String
mkToken :: UserName -> Token
mkToken u = "_" ++ u ++ "_"

mkCookie :: CookieVal -> String
mkCookie c = "sec-token=" ++ show c ++ "; path=/;"

parseCookie :: String -> Maybe CookieVal
parseCookie s = lookup "sec-token" $ pairs s
  where
    pairs = catMaybes . map (toPair . splitOn "=") . splitOn ";"
    toPair (k:v:[]) = Just (trim k, v)
    toPair _ = Nothing
    -- Wow, this is fancy...
    trim = head . drop 2 . iterate (reverse . dropWhile (==' '))

private :: (UserName, CookieVal) -> Server PrivateApi
private (authUser, _) =
       test
  :<|> test2
  where
    test = do liftIO $ do putStrLn $ "test: Authuser [" ++ authUser ++ "]"
                          putStrLn "Sleeping now"
                          threadDelay $ 1000 * 1000 * 10
                          putStrLn "Finished sleep"
              return $ head users
    test2 = do liftIO $ putStrLn $ "test2: Authuser [" ++ authUser ++ "]"
               return $ head users
