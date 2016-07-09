{-# LANGUAGE LambdaCase #-}

import qualified ScanMsgProp

import System.Exit

tests :: [IO Bool]
tests = [ScanMsgProp.runTests]

main :: IO ()
main = all id <$> sequence tests >>= \case True -> exitSuccess
                                           False -> exitFailure
