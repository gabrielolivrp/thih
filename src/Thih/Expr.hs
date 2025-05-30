{-# LANGUAGE OverloadedStrings #-}

module Thih.Expr where

import Control.Monad
import Data.List (partition, union, (\\))
import Thih.Assump (Assump (..), find)
import Thih.Id (Id)
import Thih.Infer (Infer)
import Thih.Kind (Kind (Star))
import Thih.Lit (Literal, tiLit)
import Thih.Pat
import Thih.Pred (ClassEnv (defaults), Pred (IsIn), Qual (..), entail, reduce)
import Thih.Scheme (Scheme, quantify, toScheme)
import Thih.Subst (Subst, Types (apply, tv), intersect, (@@))
import Thih.TI (TI, freshInst, getSubst, newTVar, runTI, unify)
import Thih.Type (Type (TVar), Tyvar, fn)

data Expr
  = Var Id
  | Lit Literal
  | Const Assump
  | Ap Expr Expr
  | Let BindGroup Expr

tiExpr :: Infer Expr Type
tiExpr _ as (Var i) = do
  sc <- find i as
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExpr _ _ (Const (_ :>: sc)) = do
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExpr _ _ (Lit l) = do
  (ps, t) <- tiLit l
  return (ps, t)
tiExpr ce as (Ap e f) = do
  (ps, te) <- tiExpr ce as e
  (qs, tf) <- tiExpr ce as f
  t <- newTVar Star
  unify (tf `fn` t) te
  return (ps ++ qs, t)
tiExpr ce as (Let bg e) = do
  (ps, as') <- tiBindGroup ce as bg
  (qs, t) <- tiExpr ce (as' ++ as) e
  return (ps ++ qs, t)

type Alt = ([Pat], Expr)

tiAlt :: Infer Alt Type
tiAlt ce as (pats, e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExpr ce (as' ++ as) e
  pure (ps ++ qs, foldr fn t ts)

tiAlts :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
tiAlts ce as alts t = do
  psts <- mapM (tiAlt ce as) alts
  mapM_ (unify t . snd) psts
  pure (concatMap fst psts)

split :: (Monad m) => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps = do
  ps' <- reduce ce ps
  let (ds, rs) = partition (all (`elem` fs) . tv) ps'
  rs' <- defaultedPreds ce (fs ++ gs) rs
  pure (ds, rs \\ rs')

type Ambiguity = (Tyvar, [Pred])

ambiguities :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities _ vs ps = [(v, filter (elem v . tv) ps) | v <- tv ps \\ vs]

numClasses :: [Id]
numClasses = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

stdClasses :: [Id]
stdClasses = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"] ++ numClasses

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) =
  [ t'
  | let is = [i | IsIn i _ <- qs]
        ts = [t | IsIn _ t <- qs]
  , all (TVar v ==) ts
  , any (`elem` numClasses) is
  , all (`elem` stdClasses) is
  , t' <- defaults ce
  , all (entail ce []) [IsIn i t' | i <- is]
  ]

withDefaults :: (Monad m) => ([Ambiguity] -> [Type] -> a) -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
  | any null tss = error "cannot resolve ambiguity"
  | otherwise = pure $ f vps (map head tss)
 where
  vps = ambiguities ce vs ps
  tss = map (candidates ce) vps

defaultedPreds :: (Monad m) => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds = withDefaults (\vps _ -> concatMap snd vps)

defaultSubst :: (Monad m) => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst = withDefaults (zip . map fst)

type Expl = (Id, Scheme, [Alt])

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (_, sc, alts) =
  do
    (qs :=> t) <- freshInst sc
    ps <- tiAlts ce as alts t
    s <- getSubst
    let qs' = apply s qs
        t' = apply s t
        fs = tv (apply s as)
        gs = tv t' \\ fs
        sc' = quantify gs (qs' :=> t')
        ps' = filter (not . entail ce qs') (apply s ps)
    (ds, rs) <- split ce fs gs ps'
    if sc /= sc'
      then
        error "signature too general"
      else
        if not (null rs)
          then
            error "context too weak"
          else
            pure ds

type Impl = (Id, [Alt])

restricted :: [Impl] -> Bool
restricted = any simple
 where
  simple (_, alts) = any (null . fst) alts

tiImpls :: Infer [Impl] [Assump]
tiImpls ce as bs = do
  ts <- mapM (\_ -> newTVar Star) bs
  let is = map fst bs
      scs = map toScheme ts
      as' = zipWith (:>:) is scs ++ as
      altss = map snd bs
  pss <- zipWithM (tiAlts ce as') altss ts
  s <- getSubst
  let ps' = apply s (concat pss)
      ts' = apply s ts
      fs = tv (apply s as)
      vss = map tv ts'
      gs = foldr1 union vss \\ fs
  (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
  if restricted bs
    then
      let gs' = gs \\ tv rs
          scs' = map (quantify gs' . ([] :=>)) ts'
       in return (ds ++ rs, zipWith (:>:) is scs')
    else
      let scs' = map (quantify gs . (rs :=>)) ts'
       in return (ds, zipWith (:>:) is scs')

type BindGroup = ([Expl], [[Impl]])

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es, iss) =
  do
    let as' = [v :>: sc | (v, sc, _) <- es]
    (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
    qss <- mapM (tiExpl ce (as'' ++ as' ++ as)) es
    return (ps ++ concat qss, as'' ++ as')

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq _ _ _ [] = return ([], [])
tiSeq ti ce as (bs : bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' ++ as) bss
  return (ps ++ qs, as'' ++ as')

type Program = [BindGroup]

tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram ce as bgs = runTI $
  do
    (ps, as') <- tiSeq tiBindGroup ce as bgs
    s <- getSubst
    rs <- reduce ce (apply s ps)
    s' <- defaultSubst ce [] rs
    return (apply (s' @@ s) as')
