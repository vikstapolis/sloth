module Evaluate.Expression where

import Syntax
import Evaluate.Internal
import Control.Monad.Trans.State (StateT)

evalExp :: Expression
        -> StateT Store IO Literal