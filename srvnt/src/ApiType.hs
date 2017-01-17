{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module ApiType where

--import Data.Text
import Servant.API
import qualified Servant as S

--import Control.Monad.Except
--import Control.Monad.Reader
--import Data.Aeson.Compat
import Data.Aeson.Types
--import Data.Attoparsec.ByteString
--import qualified Data.ByteString.Char8 as BS (pack) -- OverloadedStrings
--import Data.List
--import Data.Maybe
--import Data.String.Conversions
--import Data.Time.Calendar
import GHC.Generics
--import Lucid
--import Network.HTTP.Media ((//), (/:))
--import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
--import System.Directory
--import Text.Blaze
--import Text.Blaze.Html.Renderer.Utf8
--import qualified Data.Aeson.Parser
--import qualified Text.Blaze.Html
import qualified Network.Wai.Internal as WI
import qualified Network.HTTP.Types.Status as TS
import qualified Network.HTTP.Types.Header as TH
import qualified Blaze.ByteString.Builder.ByteString as BL


{-
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Prelude ()
import Prelude.Compat
-}


--type UserAPI = "users" :> BasicAuth "foo-realm" User :> Get '[JSON] [User]
type UserAPI = "users" :> Get '[JSON] [User]


data User = User { name :: String
                 , age :: Int
                 }
          deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users = [ User "Paul" 32
        , User "Joe" 44
        ]

server :: S.Server UserAPI
server = return users

userAPI :: S.Proxy UserAPI
userAPI = S.Proxy

app :: S.Application
app = S.serve userAPI server

app2 :: S.Application
app2 req f = do
  putStrLn "============> Request dump start"
  putStrLn $ show req
  putStrLn "============> Request dump stop"
  f resp
  where
    resp = WI.ResponseBuilder TS.status401
                            --  [(TH.hContentType, "text/html")] $
                              [("WWW-Authenticate", "Basic realm=\"myRealm\"")] $
--WWW-Authenticate: Basic realm="myRealm"
                              BL.fromByteString "<H1>Test</H1>"

----------------------
go :: IO ()
go = do
  putStrLn "Running go"
  Warp.run 2222 app2
