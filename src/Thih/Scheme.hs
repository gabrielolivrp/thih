module Thih.Scheme where

import Thih.Kind
import Thih.Pred
import Thih.Subst
import Thih.Type

-- | A polymorphic type with universal quantification.
data Scheme = Forall [Kind] (Qual Type)
  deriving (Show, Eq)

instance Types Scheme where
  apply s (Forall ks q) = Forall ks (apply s q)
  tv (Forall _ qt) = tv qt

{- | Converts a monomorphic type into a polymorphic scheme by quantifying
 type variables and renaming them to generic indices.
-}
quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where
  vs' = [v | v <- tv qt, v `elem` vs]
  ks = map kind vs'
  s = zip vs' (map TGen [0 ..])

-- | Converts a Type into a Scheme with no quantifiers or constraints.
toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)
