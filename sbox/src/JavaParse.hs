{-# LANGUAGE QuasiQuotes #-}
module JavaParse where

import Language.Java.Parser
import Text.InterpolatedString.Perl6

go :: IO ()
go = do
  let j = [q|
import java.util.*;

public class MyClass {
  public static void main(String[] args) {
     CALL_NOT_ALLOWED(1313, "hdfs://denied");
  }
}
|]
  putStrLn j
  let x = parser compilationUnit j
  --let x = parser compilationUnit "import java.util.*; public class MyClass {}"
  putStrLn $ show x
