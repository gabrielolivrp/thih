module Thih.Kind where

{- | 'Kind' represents the "type of a type" — that is, how many type arguments a type takes.
Kinds help ensure type-level expressions are well-formed.
Common kinds include:
  *           — the kind of all concrete types (e.g., Int, Bool)
  * -> *      — the kind of type constructors that take one type argument (e.g., Maybe)
  * -> * -> * — the kind of type constructors that take two arguments (e.g., Either)
-}
data Kind
  = -- | Represents the kind of all standard (fully applied) types.
    Star
  | -- | Represents a function from one kind to another.
    Kfun Kind Kind
  deriving (Eq)

instance Show Kind where
  show Star = "*"
  show (Kfun k1 k2) = "(" <> show k1 <> " -> " <> show k2 <> ")"

class HasKind a where
  kind :: a -> Kind
