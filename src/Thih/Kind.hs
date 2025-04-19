module Thih.Kind where

data Kind
  = Star
  | Kfun Kind Kind
  deriving (Eq)

instance Show Kind where
  show Star = "*"
  show (Kfun k1 k2) = "(" <> show k1 <> " -> " <> show k2 <> ")"

class HasKind a where
  kind :: a -> Kind
