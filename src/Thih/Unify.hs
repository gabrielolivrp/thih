module Thih.Unify where

import Thih.Kind (HasKind (kind))
import Thih.Subst (Subst, Types (tv), apply, emptySubst, merge, (+->), (@@))
import Thih.Type (Type (..), Tyvar)

{- | Compute the most general unifier (MGU) of two types.
Returns a substitution that, when applied to both input types, makes them equal.
This function recursively unifies type applications, variables, and constructors.
Throws an error if the types cannot be unified.

Example:
  mgu (Maybe a) (Maybe Int)  ==>  [(a, Int)]
-}
mgu :: (Monad m) => Type -> Type -> m Subst
mgu (TAp funct argum) (TAp funct' argum') = do
  s1 <- mgu funct funct'
  s2 <- mgu (apply s1 argum) (apply s1 argum')
  pure (s2 @@ s1)
mgu (TVar v) t = varBind v t
mgu t (TVar v) = varBind v t
mgu (TCon c) (TCon c')
  | c == c' = pure emptySubst
mgu _ _ = error "Types do not unify"

{- | Bind a type variable to a type, ensuring that:
  - It does not occur within the type (occurs check).
  - The kinds of the variable and type match.
If valid, returns a substitution that maps the variable to the type.
Throws an error otherwise.

Example:
  varBind a Int  ==>  [(a, Int)]
  varBind a a    ==>  [] (no-op)
  varBind a (Maybe a) ==> error (occurs check fails)
  varBind a (Int -> Bool) ==> error (kind mismatch)
-}
varBind :: (Monad m) => Tyvar -> Type -> m Subst
varBind u t
  | t == TVar u = pure emptySubst
  | u `elem` tv t = error "Occurs check fails"
  | kind u /= kind t = error "Kind mismatch"
  | otherwise = pure (u +-> t)

{- | Attempt to match one type to another.
This is like unification, but more restricted:
it allows variables only in the first type to be substituted.
Useful for type pattern matching rather than inference.

Example:
  match (Maybe a) (Maybe Int)  ==>  [(a, Int)]
  match (a -> Int) (Int -> Bool) ==> error mismatch
-}
match :: (Monad m) => Type -> Type -> m Subst
match (TAp funct argum) (TAp funct' argum') = do
  s1 <- match funct funct'
  s2 <- match (apply s1 argum) (apply s1 argum')
  merge s2 s1
match (TVar u) t | kind u == kind t = pure (u +-> t)
match (TCon tc1) (TCon tc2)
  | tc1 == tc2 = pure emptySubst
match _ _ = error "Types do not match"
