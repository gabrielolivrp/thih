{-# LANGUAGE OverloadedStrings #-}

module Thih.Lit where

import Thih.Kind (Kind (Star))
import Thih.Pred (Pred (IsIn))
import Thih.TI (TI, newTVar)
import Thih.Type (Type, tChar, tString)

data Literal
  = LitInt Integer
  | LitChar Char
  | LitRat Rational
  | LitStr String
  deriving (Eq)

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LitChar _) = pure ([], tChar)
tiLit (LitInt _) = do
  v <- newTVar Star
  pure ([IsIn "Num" v], v)
tiLit (LitStr _) = pure ([], tString)
tiLit (LitRat _) = do
  v <- newTVar Star
  pure ([IsIn "Fractional" v], v)
