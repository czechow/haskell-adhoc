module MultiMap where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type MultiMap k v = M.Map k (S.Set v)

insert :: k -> v -> MultiMap k v -> MultiMap k v
insert k v = undefined k v

delVal :: k -> MultiMap k v -> MultiMap k v
delVal k = undefined k
