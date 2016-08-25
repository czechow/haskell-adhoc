import qualified Erbic.Data.Msg.ScanMsgProp as ScanMsgProp
import qualified MonadicProp

import System.Exit

tests :: [IO Bool]
tests = [ScanMsgProp.runTests, MonadicProp.runTests]

main :: IO ()
main = all id <$> sequence tests >>= exit

exit :: Bool -> IO ()
exit True = exitSuccess
exit False = exitFailure
