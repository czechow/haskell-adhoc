{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C

foreign import ccall "callable_from_c" callable_from_c :: CInt -> IO ()

main :: IO ()
main = do
  putStrLn "Up and running"
  callable_from_c 997