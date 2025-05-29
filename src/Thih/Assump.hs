module Thih.Assump where

import Thih.Id
import Thih.Scheme
import Thih.Subst (Types (apply, tv))

-- | Represents a type assumption associating an identifier with a polymorphic type
data Assump = Id :>: Scheme
  deriving (Show, Eq)

instance Types Assump where
  apply s (i :>: sc) = i :>: apply s sc
  tv (_ :>: sc) = tv sc

-- | Find an assumption by its identifier in a list of assumptions.
find :: (Monad m) => Id -> [Assump] -> m Scheme
find i [] = error $ "Assumption not found: " ++ show i
find i ((i' :>: sc) : as)
  | i == i' = pure sc
  | otherwise = find i as
