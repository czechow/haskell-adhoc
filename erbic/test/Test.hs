{-# LANGUAGE LambdaCase #-}

import qualified Erbic.Data.Msg.ScanMsgProp as ScanMsgProp

import System.Exit

tests :: [IO Bool]
tests = [ScanMsgProp.runTests]

main :: IO ()
main = all id <$> sequence tests >>= \case True -> exitSuccess
                                           False -> exitFailure
