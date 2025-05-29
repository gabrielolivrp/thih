module Thih.Infer where

import Thih.Assump
import Thih.Pred
import Thih.TI

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)
