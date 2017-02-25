{-# LANGUAGE RankNTypes #-}
module RankN where


data Req a = R1 a
           | R2

data Request = Request String
data Response = Response String
data RespDelvd = RespDelvd
--instance Show (Req a)


logic :: Request -> Response
logic (Request s) = Response s


mkRequest :: Show a => String -> (Req a -> Request)
mkRequest _ (R1 x) = let s = invoke "" x
                     in Request s
mkRequest _ R2 = Request "R2"

interact :: Request -> (Response -> RespDelvd) -> RespDelvd
interact req f = let resp = logic req
                 in f resp

invoke :: Show a => String -> (a -> String)
invoke _ x = show x


go :: IO ()
go = do
  let req = R1 (3 :: Int)
  let (Request body) = mkRequest "" req
  putStrLn $ "Body: " ++ body
