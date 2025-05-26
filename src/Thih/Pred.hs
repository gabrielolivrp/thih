{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Thih.Pred where

import Control.Monad (msum)
import Thih.Id (Id)
import Thih.Subst (Subst, Types (..))
import Thih.Type (Type, tDouble, tInteger)
import Thih.Unify (match, mgu)

{- | A 'Pred' represents a type class constraint.
  'IsIn "Num" Int' means "Int is an instance of Num".
-}
data Pred = IsIn Id Type
  deriving (Eq)

instance Show Pred where
  show (IsIn i t) = show i <> " " <> show t

{- | A 'Qual t' represents a qualified type.
  For example, (Eq a, Show a) => a -> String is represented as:
    [IsIn "Eq" a, IsIn "Show" a] :=> (a -> String)
-}
data Qual t = [Pred] :=> t
  deriving (Eq)

instance (Show t) => Show (Qual t) where
  show (ps :=> t) = "(" <> unwords (map show ps) <> ") => " <> show t

{- | An instance declaration is a qualified predicate.
  For example: Eq a => Eq [a]
-}
type Inst = (Qual Pred)

{- | A 'Class' consists of:
  - a list of superclasses (by name),
  - and a list of instance declarations for the class.
-}
type Class = ([Id], [Inst])

instance (Types t) => Types (Qual t) where
  apply :: (Types t) => Subst -> Qual t -> Qual t
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t) = tv ps ++ tv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn _ t) = tv t

{- | The 'ClassEnv' represents the type class environment.
  It maps class names to their definitions and holds a list of default types.
-}
data ClassEnv = ClassEnv
  { classes :: Id -> Maybe Class
  , defaults :: [Type]
  }

-- | An 'EnvTransformer' is a function that updates a ClassEnv (if successful).
type EnvTransformer = ClassEnv -> Maybe ClassEnv

-- | Compose two environment transformers.
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(<:>) f g ce = do
  ce' <- f ce
  g ce'

{- | Get the list of superclasses of a class.
  Throws an error if the class is not found.
-}
super :: ClassEnv -> Id -> [Id]
super ce i
  | Just (is, _) <- classes ce i = is
  | otherwise = error $ "Class " <> show i

{- | Get the list of instance declarations for a class.
  Throws an error if the class is not found.
-}
insts :: ClassEnv -> Id -> [Inst]
insts ce i
  | Just (_, is) <- classes ce i = is
  | otherwise = error $ "Class " <> show i

-- | Replace or insert a class into the environment.
modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c =
  ce
    { classes =
        \i' ->
          if i == i'
            then Just c
            else classes ce i'
    }

-- | The initial class environment with no classes and default types Integer and Double.
initialEnv :: ClassEnv
initialEnv =
  ClassEnv
    { classes = \_ -> error "Class not found"
    , defaults = [tInteger, tDouble]
    }

{- | Add a new class with its superclasses.
  Fails if the class is already defined or if any superclass is undefined.
-}
addClass :: Id -> [Id] -> EnvTransformer
addClass i is ce
  | defined (classes ce i) = error $ "Class " <> show i <> " already defined"
  | not (all (defined . classes ce) is) = error $ "Class " <> show i <> " depends on undefined class"
  | otherwise = Just $ modify ce i (is, [])

{- | Add a new instance to a class.
  Fails if the class is undefined or if the instance overlaps with an existing one.
-}
addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
  | not (defined (classes ce i)) = error $ "Class " <> show i <> " not defined"
  | any (overlap p) qs = error $ "Instance " <> show p <> " overlaps with existing instance"
  | otherwise = pure (modify ce i c)
 where
  its = insts ce i
  qs = [q | (_ :=> q) <- its]
  c = (super ce i, (ps :=> p) : its)

-- | Check whether two predicates overlap by attempting unification.
overlap :: Pred -> Pred -> Bool
overlap p q = defined (mguPred p q)

-- | Check if a 'Maybe' value is defined.
defined :: Maybe a -> Bool
defined Nothing = False
defined (Just _) = True

-- | Attempt to unify two predicates and return their most general unifier.
mguPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu

-- | Attempt to match one predicate to another (left must be more general).
matchPred :: Pred -> Pred -> Maybe Subst
matchPred = lift match

{- | Lift a unification or matching function to work on predicates.
  Only matches if both predicates belong to the same class.
-}
lift :: (Type -> Type -> Maybe a) -> Pred -> Pred -> Maybe a
lift m (IsIn i t) (IsIn i' t')
  | i == i' = m t t'
  | otherwise = Nothing

-- | Get all predicates that are superclasses of a given predicate.
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) =
  p : concat [bySuper ce (IsIn i' t) | i' <- super ce i]

-- | Get all instances of a predicate by its class.
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i _) = msum [tryInst it | it <- insts ce i]
 where
  tryInst (ps :=> h) = do
    u <- mguPred h p
    pure (map (apply u) ps)

-- | Check if a set of predicates entails a given predicate.
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p =
  any ((p `elem`) . bySuper ce) ps
    || case byInst ce p of
      Nothing -> False
      Just qs -> all (entail ce ps) qs

-- | Add a standard set of type classes.
addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses <:> addNumClasses

addCoreClasses :: EnvTransformer
addCoreClasses =
  addClass "Eq" []
    <:> addClass "Ord" ["Eq"]
    <:> addClass "Show" []
    <:> addClass "Read" []
    <:> addClass "Bounded" []
    <:> addClass "Enum" []
    <:> addClass "Functor" []
    <:> addClass "Monad" []

addNumClasses :: EnvTransformer
addNumClasses =
  addClass "Num" ["Eq", "Show"]
    <:> addClass "Real" ["Num", "Ord"]
    <:> addClass "Fractional" ["Num"]
    <:> addClass "Integral" ["Real", "Enum"]
    <:> addClass "RealFrac" ["Real", "Fractional"]
    <:> addClass "Floating" ["Fractional"]
    <:> addClass "RealFloat" ["RealFrac", "Floating"]
