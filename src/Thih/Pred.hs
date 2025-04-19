module Thih.Pred where

import Thih.Id (Id)
import Thih.Subst (Subst, Types (..))
import Thih.Type (Type)
import Thih.Unify (match, mgu)

data Pred = IsIn Id Type
  deriving (Eq)

instance Show Pred where
  show (IsIn i t) = show i <> " " <> show t

data Qual t = [Pred] :=> t
  deriving (Eq)

instance (Show t) => Show (Qual t) where
  show (ps :=> t) = "(" <> unwords (map show ps) <> ") => " <> show t

type Class = ([Id], [Inst])

type Inst = (Qual Pred)

instance (Types t) => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t) = tv ps ++ tv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn _ t) = tv t

mguPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu

matchPred :: Pred -> Pred -> Maybe Subst
matchPred = lift match

lift :: (Type -> Type -> Maybe a) -> Pred -> Pred -> Maybe a
lift m (IsIn i t) (IsIn i' t')
  | i == i' = m t t'
  | otherwise = Nothing
