{-# LANGUAGE OverloadedStrings #-}

module Thih.Type where

import Thih.Id
import Thih.Kind

data Tyvar = Tyvar Id Kind
  deriving (Eq)

instance Show Tyvar where
  show (Tyvar tvar k) = show tvar <> " :: " <> show k

data Tycon = Tycon Id Kind
  deriving (Eq)

instance Show Tycon where
  show (Tycon tcon k) = show tcon <> " :: " <> show k

data Type
  = TVar Tyvar
  | TCon Tycon
  | TAp Type Type
  | TGen Int
  deriving (Eq)

instance Show Type where
  show (TVar v) = show v
  show (TCon c) = show c
  show (TAp t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"
  show (TGen n) = "t" <> show n

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind (TVar v) = kind v
  kind (TCon c) = kind c
  kind (TAp funct _) = case kind funct of
    Kfun _ body -> body
    _ -> error "Kind mismatch"
  kind (TGen _) = Star

infixr 4 `fn`
fn :: Type -> Type -> Type
fn t1 = TAp (TAp tArrow t1)

list :: Type -> Type
list = TAp tList

pair :: Type -> Type -> Type
pair t1 = TAp (TAp tTuple2 t1)

tUnit :: Type
tUnit = TCon (Tycon "()" Star)

tChar :: Type
tChar = TCon (Tycon "Char" Star)

tInt :: Type
tInt = TCon (Tycon "Int" Star)

tInteger :: Type
tInteger = TCon (Tycon "Integer" Star)

tFloat :: Type
tFloat = TCon (Tycon "Float" Star)

tDouble :: Type
tDouble = TCon (Tycon "Double" Star)

tList :: Type
tList = TCon (Tycon "[]" (Kfun Star (Kfun Star Star)))

tArrow :: Type
tArrow = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))

tTuple2 :: Type
tTuple2 = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

tString :: Type
tString = list tChar
