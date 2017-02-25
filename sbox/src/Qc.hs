{-# LANGUAGE LambdaCase #-}

module Qc where


import Test.QuickCheck
import Control.Monad.State
import Test.QuickCheck.Monadic
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.Random (newStdGen, randomRs)
import Data.IORef -- FIXME

type LioState = State (S.Set Int)

data Lio = Li Int
         | Lo Int
         deriving Show


prop :: Property
prop = once $ forAll (mapM myGen [1..8]) $ \_ ->
--  x > 8 ==> x > 2
  True

propM :: PropertyM LioState Int
propM = run $ state $ \s -> (13, S.insert 1 s)

prop2 :: Property
prop2 = once $ monadic (\x -> evalState x S.empty) $ do
  x <- pick (myGen 1)
  monitor (counterexample $ "Failing input:" ++ show x)
  case x of
    Li i -> do run $ do state $ \s -> ((), S.insert i s)
    Lo _ -> do assert $ False == True

prop3 :: Property
prop3 = once $ monadicIO $ do
  x <- pick $ mapM myGen [1 .. 32]
  monitor $ counterexample $ "Failing input:" ++ (show $ head x)
  assert $ False == False

data Application a = Application { livy :: a }

newtype HttpCode = HttpCode Int deriving (Eq, Show)

type UserName = String
type Password = String
type AuthToken = String

type AtToUn = M.Map AuthToken UserName

data Logic = Logic (IORef AtToUn)

config :: M.Map UserName Password
config = M.fromList [("un", "pw")]

class HasSecApi l where
  login :: l -> (UserName, Password) -> IO (HttpCode, Maybe AuthToken)
  logout :: l -> AuthToken -> IO HttpCode
  checkAuth :: l -> AuthToken -> IO (Maybe UserName)

instance HasSecApi Logic where
  login (Logic atToUnRef) (un, pw) =
    case (pw ==) <$> M.lookup un config of
      Just True -> do
        at <- take 8 . randomRs ('A', 'z') <$> newStdGen
        atomicModifyIORef' atToUnRef $ \m -> (M.insert at un m, ())
        return (HttpCode 200, Just at)
      Just False -> return (HttpCode 403, Nothing)
      Nothing -> return (HttpCode 403, Nothing)

  logout l@(Logic atToUnRef) at = checkAuth l at >>= \case
    Just _ -> atomicModifyIORef' atToUnRef $ \m -> (M.delete at m, HttpCode 200)
    Nothing -> return $ HttpCode 403

  checkAuth (Logic atToUnRef) at = M.lookup at <$> readIORef atToUnRef

prop' :: (HasSecApi a) => Application a -> (Lio -> Property)
prop' app s@(Li i) =
  counterexample ("At input: " ++ show s) $ ioProperty $ do
    (code, at) <- login (livy app) ("un", "pw")
    -- Now, trace the state changes as well
    return $ code === HttpCode 200

prop' app s@(Lo i) = ioProperty $ do
  code <- logout (livy app) ("at")
  return $ counterexample ("At input: " ++ show s) $ code === HttpCode 200



type MyS = StateT (M.Map Int AuthToken) IO

prop'' :: (HasSecApi a) => Application a -> Lio -> PropertyM MyS ()
prop'' app s@(Li i) = do
  (code, at'm) <- liftIO $ login (livy app) ("un", "pw")
  myAssert ("At input: " ++ show s) code $ HttpCode 200
  case at'm of
    Just at -> run $ state $ \m -> ((), M.insert i at m)
    Nothing -> return ()

prop'' app s@(Lo i) = do
  (expCode, at'm) <- run $ state $ \m -> case M.lookup i m of
    Just at -> ((HttpCode 200, Just at), M.delete i m)
    Nothing -> ((HttpCode 403, Nothing), m)
  case at'm of
    Just at -> do code <- liftIO $ logout (livy app) at
                  myAssert ("At input: " ++ show s) code $ expCode
    Nothing -> do code <- liftIO $ logout (livy app) "<unknown at>"
                  myAssert ("At input: " ++ show s) code $ expCode


myAssert :: (Eq a, Show a, Monad m) => String -> a -> a -> PropertyM m ()
myAssert d x y
  | x == y = assert $ x == y
  | otherwise = do
      monitor $ counterexample d
      monitor $ counterexample (show x ++ " /= " ++ show y)
      assert $ x == y

propAll :: (HasSecApi a) => Application a -> Property
propAll app =
  forAll (mapM myGen [1..4]) $ \lios -> conjoin $ map (prop' app) lios

-- (do
--   x <- run $ state $ \s -> (13, S.insert 1 s)
--   return x

myGen :: Int -> Gen Lio
myGen  i = f <$> elements [minBound .. maxBound]
  where
    f True  = Li i
    f False = Lo i


go :: IO ()
go = do
  atToUnRef <- newIORef M.empty
--  quickCheck $ propAll (Application (Logic atToUnRef))
  let x = once $ monadic runner $
          mapM (prop'' (Application (Logic atToUnRef)))
          [Li 0, Li 1, Lo 0, Lo 1, Lo 3]
  quickCheck x
  where
    runner :: MyS Property -> Property
    runner m = ioProperty $ evalStateT m M.empty
    -- runner m = ioProperty $ do
    --   (p', st) <- runStateT m M.empty
    --   putStrLn $ "State is: " ++ show st
    --   return p'
