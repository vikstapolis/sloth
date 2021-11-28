module Evaluate.Block where

import Syntax
import Evaluate.Internal
import Evaluate.Statement

import Control.Monad.State (StateT)
import qualified Data.HashTable.IO as HT

evalBlock :: Block -> StateT Store IO (Maybe Literal)
evalBlock []     = return Nothing
evalBlock (x:xs) = do
    maybeReturnVal <- evalStmt x
    case maybeReturnVal of Just val -> return (Just val)
                           Nothing  -> evalBlock xs