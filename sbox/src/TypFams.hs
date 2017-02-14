{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module TypFams where

class Sum a b where
  type SumTy a b :: *
  plus :: a -> b -> SumTy a b

instance Sum Integer Double where
  type SumTy Integer Double = Double
  plus i d = fromIntegral i + d

instance Sum Double Integer where
  type SumTy Double Integer = Double
  plus d i = d + fromIntegral i

instance (Num a) => Sum a a where
  type SumTy a a = a
  plus x y = x + y
