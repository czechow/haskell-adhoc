{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: Remove instance definitions once Aeson version is 1.*
--       can be used with stack
-- Note: Version 0.11.* does not provide instance ToJSON/FromJSON of Map k v

module Spike where

import GHC.Generics
import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

import qualified MultiMap as MM

import Data.Foldable (foldlM)

--import Debug.Trace
--import Data.Dynamic
import Data.Text
import Text.Read (readMaybe)


data ResType = CpuCnt | MemSize | SessCnt
             deriving Show


class ToStrKey a where
  toStrKey :: a -> Text

class FromStrKey a where
  fromStrKey :: Text -> Either String a


instance ToStrKey Int where
  toStrKey = pack . show

instance FromStrKey Int where
  fromStrKey = read . unpack


newtype UserName = UserName String
                   deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

unUserName :: UserName -> String
unUserName (UserName x) = x

instance ToStrKey UserName where
  toStrKey = pack . unUserName

instance FromStrKey UserName where
  fromStrKey = return . UserName . unpack


data Resource = Resource { userSessionMMap :: MM.MultiMap UserName SessionId }
              deriving (Show, Generic, ToJSON, FromJSON)

newtype SessionId = SessionId Int
                  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

unSessionId :: SessionId -> Int
unSessionId (SessionId x) = x

instance ToStrKey SessionId where
  toStrKey = pack . show . unSessionId

instance FromStrKey SessionId where
  fromStrKey x = case readMaybe $ unpack x of
    Just x' -> Right $ SessionId x'
    Nothing -> Left $ "Unable to convert key [" ++ unpack x ++ "] to SessionId"

-- Orphan instances here:
instance (ToStrKey k, ToJSON v) => ToJSON (M.Map k v) where
  toJSON = Object . M.foldlWithKey' f H.empty
    where
      f m k v = H.insert (toStrKey k) (toJSON v) m

instance (Ord k, FromStrKey k, FromJSON v) => FromJSON (M.Map k v) where
  parseJSON (Object m) = foldlM f M.empty $ H.keys m
    where
      f m' k = do v <- m .: k
                  case fromStrKey k of
                    Right k' -> return $ M.insert k' v m'
                    Left e -> fail e
  parseJSON _ = fail "Fail here"


sp2 :: M.Map Int String
sp2 = M.fromList [(1, "Pawel"), (2, "Gawel")]

sp3 :: M.Map SessionId String
sp3 = M.fromList [(SessionId 1, "Pawel"), (SessionId 1313, "Gawel")]

r :: MM.MultiMap UserName SessionId
r = M.fromList [(UserName "Dr Who", S.fromList [SessionId 1, SessionId 13])]


res :: Resource
res = Resource r


go :: IO ()
go = do
  let x = MM.insert undefined
  return ()


go2 :: Either String (Int, Int, Int)
go2 = do
  x <- Right 4
  y <- Left $ "err " ++ show x
  z <- Right 5
  return (x, y, z)
