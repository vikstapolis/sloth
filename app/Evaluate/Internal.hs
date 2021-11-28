module Evaluate.Internal where

import Syntax
import Data.HashTable.IO (BasicHashTable)
import Control.Monad.State (StateT, get, lift, runStateT)

type HashTable k v = BasicHashTable k v
data Store = Store { localVars :: HashTable String Literal
                   , globalVars :: HashTable String Literal
                   }

local :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
local f g = do
    st <- get
    lift $ fst <$> runStateT g (f st)