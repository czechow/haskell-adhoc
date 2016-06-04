module STmonad where

import Control.Monad.ST
import Data.STRef


sum :: Num a => [a] -> a
sum xs = runST $ do
  n <- newSTRef 0
  mapM_ (\s -> do
            x <- readSTRef n
            writeSTRef n (x + s)
        ) xs
  readSTRef n
