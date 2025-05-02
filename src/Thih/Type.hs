{-# LANGUAGE OverloadedStrings #-}

module Thih.Type where

import Thih.Id
import Thih.Kind

{- | A type variable, represented by an identifier and its kind.
Example: a :: *
-}
data Tyvar = Tyvar Id Kind
  deriving (Eq)

instance Show Tyvar where
  show (Tyvar tvar k) = show tvar <> " :: " <> show k

{- | A type constructor, which produces types given arguments, represented by its name and kind.
Example: Maybe :: * -> *, (->) :: * -> * -> *
-}
data Tycon = Tycon Id Kind
  deriving (Eq)

instance Show Tycon where
  show (Tycon tcon k) = show tcon <> " :: " <> show k

-- | The core 'Type' data type, representing type expressions.
data Type
  = -- | A type variable, such as a, b
    TVar Tyvar
  | -- | A type constructor, such as Int, Maybe
    TCon Tycon
  | -- | Type application. For example, TAp (TCon Maybe) (TVar a) represents Maybe a
    TAp Type Type
  | -- | A generalized type variable used in type schemes (e.g., t0, t1)
    TGen Int
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
  kind (TAp f _) = case kind f of
    Kfun _ result -> result
    _ -> error "Kind mismatch in type application"
  kind (TGen _) = Star

fn :: Type -> Type -> Type
fn t1 = TAp (TAp tArrow t1)

infixr 4 `fn`

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
tList = TCon (Tycon "[]" (Kfun Star Star))

tArrow :: Type
tArrow = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))

tTuple2 :: Type
tTuple2 = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

tString :: Type
tString = list tChar
