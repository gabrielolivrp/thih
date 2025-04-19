module Thih.Subst where

import Thih.Type

type Subst = [(Tyvar, Type)]

emptySubst :: Subst
emptySubst = []

(+->) :: Tyvar -> Type -> Subst
(+->) v t = [(v, t)]

class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Tyvar]

instance Types Type where
  apply s (TVar v) =
    case lookup v s of
      Just t -> t
      Nothing -> TVar v
  apply s (TAp funct argum) = TAp (apply s funct) (apply s argum)
  apply _ t = t

  tv (TVar v) = [v]
  tv (TAp funct argum) = tv funct ++ tv argum
  tv _ = []

instance (Types a) => Types [a] where
  apply s = map (apply s)
  tv = concatMap tv

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge :: (Monad m) => Subst -> Subst -> m Subst
merge s1 s2
  | agree = pure (s1 ++ s2)
  | otherwise = error "incompatible substitutions"
 where
  agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) (map fst s1 `intersect` map fst s2)

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] = const []
intersect xs = filter (`elem` xs)
