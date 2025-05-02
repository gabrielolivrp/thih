module Thih.Subst where

import Thih.Type

{- | A substitution is a list of pairs mapping type variables to types.
For example, the substitution [(a, Int), (b, Bool)] replaces 'a' with Int and 'b' with Bool.
-}
type Subst = [(Tyvar, Type)]

emptySubst :: Subst
emptySubst = []

{- | Creates a substitution that maps a single type variable to a type.
Example: a +-> Int == [(a, Int)]
-}
(+->) :: Tyvar -> Type -> Subst
(+->) v t = [(v, t)]

{- | The 'Types' type class defines types that can have substitutions applied to them,
and from which free type variables can be extracted.
-}
class Types t where
  -- | Apply a substitution to a value of type 't'.
  -- Example: apply [(a, Int), (b, Bool)] (a -> b) == Int -> Bool
  apply :: Subst -> t -> t

  -- | Return a list of free type variables in a value of type 't'.
  tv :: t -> [Tyvar]

{- | Instance of 'Types' for the 'Type' type.
This allows applying substitutions to types and extracting their free variables.
-}
instance Types Type where
  apply s (TVar v) =
    case lookup v s of
      Just t -> t
      Nothing -> TVar v
  apply s (TAp f a) = TAp (apply s f) (apply s a)
  apply _ t = t

  tv (TVar v) = [v]
  tv (TAp f a) = tv f ++ tv a
  tv _ = []

-- | Apply a substitution to each element in a list and gather free variables from all elements.
instance (Types a) => Types [a] where
  apply s = map (apply s)
  tv = concatMap tv

{- | Compose two substitutions. The result applies the first substitution to the second,
then concatenates the result with the first substitution.
Example:
  s1 = [(a, Int)]
  s2 = [(b, a)]
  s1 @@ s2 == [(b, Int), (a, Int)]
This ensures any variables in s2 that refer to variables replaced by s1 are also updated.
-}
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

infixr 4 @@

{- | Merge two substitutions, ensuring they are consistent (i.e., agree on shared variables).
Returns a combined substitution if compatible, otherwise raises an error.
Example:
  merge [(a, Int)] [(b, Bool)] == [(a, Int), (b, Bool)]
  merge [(a, Int)] [(a, Int)] == [(a, Int), (a, Int)]
  merge [(a, Int)] [(a, Bool)] -> error: incompatible substitutions
-}
merge :: (Monad m) => Subst -> Subst -> m Subst
merge s1 s2
  | agree = pure (s1 ++ s2)
  | otherwise = error "incompatible substitutions"
 where
  agree =
    all
      (\v -> apply s1 (TVar v) == apply s2 (TVar v))
      (map fst s1 `intersect` map fst s2)

{- | Computes the intersection of two lists, keeping only elements found in both.
Used for checking conflicting variable mappings in 'merge'.
-}
intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] = const []
intersect xs = filter (`elem` xs)
