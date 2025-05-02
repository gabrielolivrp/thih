{-# LANGUAGE ImportQualifiedPost #-}

module Thih.Id where

import Data.Text qualified as T

-- | An 'Id' represents identifiers such as variable names, type constructor names, or type variable names.
type Id = T.Text

-- | Generate a fresh identifier based on an integer.
enumId :: Int -> Id
enumId n = T.pack $ "v" <> show n
