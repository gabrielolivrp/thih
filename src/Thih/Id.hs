{-# LANGUAGE ImportQualifiedPost #-}

module Thih.Id where

import Data.Text qualified as T

type Id = T.Text

enumId :: Int -> Id
enumId n = T.pack $ "v" <> show n
