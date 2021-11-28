module Evaluate.Block where

import Syntax ( Literal, Block )
import Evaluate.Internal ( Store )
import Control.Monad.State (StateT)

evalBlock :: Block -> StateT Store IO (Maybe Literal)