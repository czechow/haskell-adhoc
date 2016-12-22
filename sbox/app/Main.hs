module Main where


import Criterion.Main
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary (Arbitrary)
import Control.Monad.Except
import Control.Concurrent

--import MyFree

fib :: Integer -> Integer
fib m | m < 0 = error "negative!"
      | m == 0 = 0
      | m == 1 = 1
      | otherwise = fib (m-1) + fib (m-2)

--main :: IO ()
--main = putStrLn "Up and running"


main :: IO ()
main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "11" $ whnf fib 11
               , bench "30" $ whnf fib 30
               ]
  ]

data TestStatement = TS { code :: String
                        , timeout :: Int
                        }
                   deriving Show

mkTestStatement :: Int -> Int -> TestStatement
mkTestStatement sc pl =
  let c = "select /*+ parallel (d, " ++ show pl ++
          ") */ from d where rownum < " ++ show sc
  in TS c 3600

data TestCommand = TC TestStatement
                 | CloseSession
                 | DelaySecs Int
                 deriving Show

instance Arbitrary TestStatement where
  arbitrary = do
    sc <- QC.choose (1024, 2048)
    pl <- QC.elements $ map (2^) rng
    return $ mkTestStatement sc pl
      where
        rng = [0 .. 4] :: [Int]

instance Arbitrary TestCommand where
  arbitrary = QC.frequency [ (5, pure CloseSession)
                           , (10, DelaySecs <$> QC.choose (0, 30))
                           , (85, TC <$> QC.arbitrary )]


type IOEitherString = ExceptT String IO
data SessionStartupConf = SessionStartupConf Type Int
                        deriving Show

data Type = SPARK
          | PYTHON
          | R
          deriving Show

type SessionData = String
data Session a = Session SessionData
               deriving Show

data CREATED
data IDLE
data RUNNING
data CLOSED

newSession :: Type -> IOEitherString (Session CREATED)
newSession t = do
  liftIO $ putStrLn $ "Creating a session of type [" ++ show t ++ "]"
  return $ Session ""

type SessionId = Int
type SessionStatus = String

sessionStatus :: SessionId -> IOEitherString SessionStatus
sessionStatus sId = do
  let x = liftIO $ (do putStrLn $ "Fake request to HTTP server"; return "ok")
  x

type Seconds = Int

waitForSession :: Seconds -> ((Session CREATED) -> Bool) -> Session CREATED
               -> IOEitherString (Session RUNNING)
waitForSession 0 _ _ = throwError "Timed out waiting for session"
waitForSession n p s@(Session ds)
  | p s = return $ Session ds
  | otherwise = do
      liftIO $ threadDelay $ 1000 * 1000
      undefined

oper1 :: String -> IOEitherString Int
oper1 x = do
  liftIO $ putStrLn $ "oper1: a is [" ++ show x ++ "]"
  if (x == "df")
    then throwError "This sucks"
    else return 5

oper2 :: Show a => a -> IOEitherString String
oper2 x = do
  liftIO $ putStrLn $ "oper2: a is [" ++ show x ++ "]"
  return $ show x


myFun :: String -> IO ()
myFun x = do
  let z1 = runExceptT $ (oper1 >=> oper2 >=> oper2 >=> oper2) x
  --x <- runExceptT $ oper1 "x"
  y <- z1
  putStrLn $ "y is " ++ show y
