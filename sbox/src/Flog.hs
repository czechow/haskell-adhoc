{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Flog where

import Prelude hiding (log)
import System.Log.FastLogger
import Data.Monoid ((<>))
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)


-- data Loc
--   = Loc { loc_filename :: String
--     , loc_package  :: String
--     , loc_module   :: String
--     , loc_start    :: CharPos
--     , loc_end      :: CharPos }
-- type CharPos = (Int, Int)

-- rootLogger :: FastLogger
-- rootLogger =
--   let l = fst $ unsafePerformIO $ newFastLogger (LogStdout 128)
--   in unsafePerformIO $ newIORef l

saveLogger :: FastLogger -> IO ()
saveLogger l = undefined

go :: IO ()
go = withFastLogger (LogStdout 128) $ \l -> do
  saveLogger l


liftLoc' :: Loc -> Q Exp
liftLoc' (Loc a b c (_, _) (_, _)) = [|$(lift a)|]
-- Since 0.3.1
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(lift a)
    $(lift b)
    $(lift c)
    ($(lift d1), $(lift d2))
    ($(lift e1), $(lift e2))
    |]
-- go :: IO ()
-- go = withTimedFastLogger (return "Time") (LogStdout 128) $ \l -> do
--   log l "test"
--   log l "test2"

loggerH :: Loc -> IO ()
loggerH l =
  putStrLn $ "Location is ===> " ++ show l

logTH :: Q Exp
logTH = [|loggerH $(qLocation >>= liftLoc)|]

-- log :: TimedFastLogger -> LogStr -> IO ()
-- log tfl msg = tfl $ do \ft -> toLogStr ft <> ": " <> msg <> "\n"

go2 :: IO ()
go2 = do
  --putStrLn $ show $(logTH)
  undefined
