{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Rest where


import Prelude hiding (id)
import GHC.Generics
import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Char (toLower, toUpper)
import Text.Read (readMaybe)


newtype SessionId = SessionId Int
                  deriving (Show, Eq, Ord, Generic)

unSessionId :: SessionId -> Int
unSessionId (SessionId v) = v


data SessionKind = SPARK
                 | PYSPARK
                 | PYSPARK3
                 | SPARKR
                 deriving (Show, Read, Generic)

instance FromJSON SessionKind where
  parseJSON (String s) = case readMaybe $ map toUpper $ T.unpack s of
    Just s' -> pure s'
    Nothing -> fail $ "Cannot parse value into SessionKind"
  parseJSON _ = fail $ "Cannot parse value into SessionKind"

instance ToJSON SessionKind where
  toJSON sk = String $ T.pack $ map toLower $ show sk


newtype UserName = UserName String
                 deriving (Show, Generic)

instance FromJSON UserName
instance ToJSON UserName


-- Used in creating sessions PUT /sessions
data SessionConfig = SessionConfig { kind :: SessionKind
                                   , proxyUser :: Maybe UserName
                                   , config :: Maybe (M.Map String String)
                                   }
                   deriving (Show, Generic)

instance FromJSON SessionConfig


data Session a = Session { id :: SessionId
                         , kind :: SessionKind
                         , proxyUser :: Maybe UserName
                         , config :: Maybe (M.Map String String)
                         }
               deriving (Show, Generic)

instance ToJSON SessionId
instance FromJSON SessionId
instance ToJSON (Session a)
instance FromJSON (Session a)


-- Used in response to GET /sessions
data Sessions a = Sessions { total :: Int
                           , sessions :: [Session a]
                           }
                deriving (Show, Generic)

instance ToJSON (Sessions a)

data SessionDeleted = SessionDeleted { msg :: String }
                    deriving (Show, Generic)

instance ToJSON SessionDeleted
