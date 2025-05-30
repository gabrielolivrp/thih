{-# LANGUAGE OverloadedStrings #-}

module Thih.Pat where

import Thih.Assump (Assump (..))
import Thih.Id (Id)
import Thih.Kind (Kind (Star))
import Thih.Lit (Literal, tiLit)
import Thih.Pred (Pred (IsIn), Qual (..))
import Thih.Scheme (toScheme)
import Thih.TI (TI, freshInst, newTVar, unify)
import Thih.Type (Type, fn)

data Pat
  = PVar Id
  | PWildcard
  | PAs Id Pat
  | PLit Literal
  | PNpk Id Integer
  | PCon Assump [Pat]

tiPat :: Pat -> TI ([Pred], [Assump], Type)
tiPat (PVar i) = do
  v <- newTVar Star
  pure ([], [i :>: toScheme v], v)
tiPat PWildcard = do
  v <- newTVar Star
  pure ([], [], v)
tiPat (PAs i p) = do
  (ps, as, t) <- tiPat p
  pure (ps, (i :>: toScheme t) : as, t)
tiPat (PLit l) = do
  (ps, t) <- tiLit l
  pure (ps, [], t)
tiPat (PNpk i _) = do
  t <- newTVar Star
  pure ([IsIn "Integral" t], [i :>: toScheme t], t)
tiPat (PCon (_ :>: sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t' <- newTVar Star
  (qs :=> t) <- freshInst sc
  unify t (foldr fn t' ts)
  pure (ps ++ qs, as, t')

tiPats :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do
  psasts <- mapM tiPat pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  pure (ps, as, ts)
