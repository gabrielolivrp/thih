module Thih.Unify where

import Thih.Kind (HasKind (kind))
import Thih.Subst (Subst, Types (tv), apply, emptySubst, merge, (+->), (@@))
import Thih.Type (Type (..), Tyvar)

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

varBind :: (Monad m) => Tyvar -> Type -> m Subst
varBind u t
  | t == TVar u = pure emptySubst
  | u `elem` tv t = error "Occurs check fails"
  | kind u /= kind t = error "Kind mismatch"
  | otherwise = pure (u +-> t)

match :: (Monad m) => Type -> Type -> m Subst
match (TAp funct argum) (TAp funct' argum') = do
  s1 <- match funct funct'
  s2 <- match (apply s1 argum) (apply s1 argum')
  merge s2 s1
match (TVar u) t | kind u == kind t = pure (u +-> t)
match (TCon tc1) (TCon tc2)
  | tc1 == tc2 = pure emptySubst
match _ _ = error "Types do not match"
