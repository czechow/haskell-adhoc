{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}

module MyFree where

--import Control.Monad.Free

data Expr = Lit Int
          | Add Expr Expr

prog :: Expr
prog = Add (Lit 1) (Add (Lit 3) (Lit 4))

eval :: Expr -> Int
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)

txt :: Expr -> String
txt (Lit i) = show i
txt (Add e1 e2) = "(" ++ txt e1 ++ " + " ++ txt e2 ++ ")"


{-
Say "Hello"
Say "Who are you?"
Ask for a "name"
Say "Nice to meet you, " ++ name

conv1 :: IO ()
conv1 = do
  say "Hello"
  say "Who are you?"
  name <- ask
  say $ "Nice to meet you, " ++ name

say :: String -> IO ()
say = putStrLn

ask :: IO String
ask = getLine
-}

-------------------------------------------------------------------------------
--                               New approach
-------------------------------------------------------------------------------

{-
data Interaction a

instance Functor Interaction where
instance Applicative Interaction

instance Monad Interaction

say :: String -> Interaction ()
ask :: Interaction String
-}


data Interaction :: * -> * where
  Say :: String -> Interaction ()
  Ask :: Interaction String

  Return :: a -> Interaction a
  Bind :: Interaction a -> (a -> Interaction b) -> Interaction b

-- (>>=) :: m a -> (a -> m b) -> m b

instance Functor Interaction where
  -- (a -> b) -> f a -> f b
  --fmap f fa = fa >>= \x -> return $ f x
  fmap f fa = pure f <*> fa

instance Applicative Interaction where
  pure = return
  -- f (a -> b) -> f a -> f b
  (<*>) fab fa = fa >>= \x -> fab >>= \ab -> return $ ab x

instance Monad Interaction where
  return = Return
  -- m a -> (a -> m b) -> m b
  (>>=) = Bind

say = Say
ask = Ask

run :: Interaction a -> IO a
run (Say msg) = putStrLn msg
run Ask = getLine
run (Return x) = return x
run (Bind ia fm) = run ia >>= \x -> run $ fm x


prog2 :: Interaction ()
prog2 = Bind
        (Bind
        (Bind (Say "What is your name?")
              (\_ -> Ask))
              (\name -> Say $ "Hello " ++ name))
              (\_ -> Say "This is the end")

simulate :: [String] -> Interaction a -> [String]
simulate _ (Say msg) = [msg]
simulate (s:ss) Ask = [s]
simulate _ (Return _) = []
simulate (s:ss) (Bind ia fm) = undefined

prog2' :: Interaction ()
prog2' = (Say "What is your name?")

{-
data Interaction' :: * -> * where
  Return' :: a -> Interaction' a
  Say' :: String -> (() -> Interaction' b) -> Interaction' b
  Ask' :: (String -> Interaction' b) -> Interaction' b
-}

data Interaction' a = Return' a
                    | Say' String (() -> Interaction' a)
                    | Ask' (String -> Interaction' a)
                    deriving (Functor)


data Free f a where
  Pure   :: a -> Free f a
  Impure :: f (Free f a) -> Free f a

eta :: Functor f => f a -> Free f a
eta fa = Impure $ fmap Pure fa
-- Free f fa
