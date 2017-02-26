{-# LANGUAGE LambdaCase #-}

module Qc where


import Test.QuickCheck
import Control.Monad.State
import Test.QuickCheck.Monadic hiding (assert)
import qualified Test.QuickCheck.Monadic as QC (assert)
import qualified Data.Map.Strict as M
import System.Random (newStdGen, randomRs)
import Data.IORef -- FIXME
import Data.Maybe (isJust, isNothing)


data LoginCreds = LcCorrect UserName Password
                | LcWrongUn Password
                | LcWrongPw UserName
                | LcNoCreds
                deriving Show

data Op = Li Int
        | Lo Int
        | Sl Int
         deriving Show

type AtState = StateT (M.Map Int AuthToken) IO

data Application a = Application { livy :: a }

newtype HttpCode = HttpCode Int deriving (Eq, Show)

type UserName = String
type Password = String
type AuthToken = String

type AtToUn = M.Map AuthToken UserName

data Logic = Logic (IORef AtToUn)


-- Logic here

class HasSecApi l where
  login :: l -> (UserName, Password) -> IO (HttpCode, Maybe AuthToken)
  logout :: l -> AuthToken -> IO HttpCode
  sessionList :: l -> AuthToken -> IO (HttpCode, [String])

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

  sessionList l at = checkAuth l at >>= \case
    Just _ -> return (HttpCode 200, [""])
    Nothing -> return (HttpCode 403, [])

  checkAuth (Logic atToUnRef) at = M.lookup at <$> readIORef atToUnRef


-- Tests here

config :: M.Map UserName Password
config = M.fromList [("un", "pw"), ("un2", "pw2")]


propSecLogin :: Property
propSecLogin = forAll loginCredsGen $ \lc -> ioProperty $ do
  app <- mkApp
  tryLogin app lc
  where
    tryLogin app (LcCorrect un pw) = do
      (code, at'm) <- login (livy app) (un, pw)
      return $ code === HttpCode 200 .&&. isJust at'm
    tryLogin app (LcWrongUn pw) = do
      (code, at'm) <- login (livy app) ("<wrong>", pw)
      return $ code === HttpCode 403 .&&. isNothing at'm
    tryLogin app (LcWrongPw un) = do
      (code, at'm) <- login (livy app) (un, "<wrong>")
      return $ code === HttpCode 403 .&&. isNothing at'm
    tryLogin app LcNoCreds = do
      (code, at'm) <- login (livy app) ("", "")
      return $ code === HttpCode 403 .&&. isNothing at'm


propSecOperM :: (HasSecApi a) => Application a -> Op -> PropertyM AtState ()
propSecOperM app s@(Li i) = do
  creds <- pick $ elements $ M.toList config
  (code, at'm) <- liftIO $ login (livy app) creds
  assertMsg ("At input: " ++ show s) code $ HttpCode 200
  case at'm of
    Just at -> run $ state $ \m -> ((), M.insert i at m)
    Nothing -> return ()

propSecOperM app s@(Lo i) = do
  (expCode, at'm) <- run $ state $ \m -> case M.lookup i m of
    Just at -> ((HttpCode 200, Just at), M.delete i m)
    Nothing -> ((HttpCode 403, Nothing), m)
  case at'm of
    Just at -> do code <- liftIO $ logout (livy app) at
                  assertMsg ("At input: " ++ show s) code expCode
    Nothing -> do code <- liftIO $ logout (livy app) "<unknown>"
                  assertMsg ("At input: " ++ show s) code expCode

propSecOperM app s@(Sl i) = (run $ M.lookup i <$> get) >>= \case
  Just at -> do (code, _) <- liftIO $ sessionList (livy app) at
                assertMsg ("At input: " ++ show s) code (HttpCode 200)
  Nothing -> do (code, _) <- liftIO $ sessionList (livy app) "<unknown>"
                assertMsg ("At input: " ++ show s) code (HttpCode 403)

propSecOpers :: Property
propSecOpers = forAll (opsGen 32) $ \lios -> ioProperty $ do
  app <- mkApp
  return $ monadic runner $ mapM (propSecOperM app) lios
  where
    runner :: AtState Property -> Property
    runner m = ioProperty $ evalStateT m M.empty


-- Generators

loginCredsGen :: Gen LoginCreds
loginCredsGen = do
  (un, pw) <- elements $ M.toList config
  elements $ [ LcCorrect un pw
             , LcWrongUn pw
             , LcWrongPw un
             , LcNoCreds
             ]

opsGen :: Int -> Gen [Op]
opsGen n = take n <$> (shuffle $ concat [lis, los, sls])
  where lis = map Li [1 .. n]
        los = map Lo [1 .. n]
        sls = map Sl [1 .. n]


-- Utilities

mkApp :: IO (Application Logic)
mkApp = do
  atToUnRef <- newIORef M.empty
  return $ Application $ Logic atToUnRef


assertMsg :: (Eq a, Show a, Monad m) => String -> a -> a -> PropertyM m ()
assertMsg d x y
  | x == y = QC.assert $ x == y
  | otherwise = do
      monitor $ counterexample d
      monitor $ counterexample (show x ++ " /= " ++ show y)
      QC.assert $ x == y


go :: IO ()
go = do
  quickCheck $ conjoin [propSecLogin, propSecOpers]
