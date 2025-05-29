{-# LANGUAGE InstanceSigs #-}

module Thih.TI where

import Thih.Id
import Thih.Kind
import Thih.Pred
import Thih.Scheme
import Thih.Subst
import Thih.Type
import Thih.Unify

newtype TI a
  = TI
      ( Subst -> -- Current substitution
        Int -> -- Next type variable index
        (Subst, Int, a) -- Resulting substitution, next index, and value
      )

instance Functor TI where
  fmap :: (a -> b) -> TI a -> TI b
  fmap f (TI g) = TI (\s n -> let (s', n', x) = g s n in (s', n', f x))

instance Applicative TI where
  pure x = TI (\s n -> (s, n, x))
  TI f <*> TI x =
    TI
      ( \s n ->
          let (s', n', g) = f s n
              (s'', n'', y) = x s' n'
           in (s'', n'', g y)
      )

instance Monad TI where
  return = pure
  TI f >>= g =
    TI
      ( \s n -> case f s n of
          (s', m, x) ->
            let TI gx = g x
             in gx s' m
      )

runTI :: TI a -> a
runTI (TI f) =
  let (_, _, x) = f emptySubst 0
   in x

getSubst :: TI Subst
getSubst = TI (\s n -> (s, n, s))

unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

extSubst :: Subst -> TI ()
extSubst s = TI (\s' n -> (s' @@ s, n, ()))

newTVar :: Kind -> TI Type
newTVar k =
  TI
    ( \s n ->
        let v = Tyvar (enumId n) k
         in (s, n + 1, TVar v)
    )

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
  ts <- mapM newTVar ks
  pure (inst ts qt)

class Instantiate t where
  inst :: [Type] -> t -> t

instance (Instantiate a) => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n) = ts !! n
  inst _ t = t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

instance (Instantiate t) => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
