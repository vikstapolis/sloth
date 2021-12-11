{-# LANGUAGE LambdaCase #-}
module Evaluate.Internal where

import Syntax

import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HT

import Control.Monad.State (StateT, get, put, lift, runStateT, liftIO, modify)

import Control.Applicative ( (<|>) )

type HashTable k v = BasicHashTable k v
data Store = Store { localVars :: [HashTable String Literal]
                   , globalVars :: HashTable String Literal
                   }

local :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
local f g = do
    st <- get
    lift $ fst <$> runStateT g (f st)

getVar :: String -> Store -> IO (Maybe Literal)
getVar name (Store loc glob) = go name loc glob
    where go name []     glob = HT.lookup glob name
          go name (x:xs) glob = (<|>)
                            <$> HT.lookup x name
                            <*> go name xs glob

isGlobalScope :: Monad m => StateT Store m Bool
isGlobalScope = do Store loc _ <- get
                   return $ null loc

addScope :: Monad m => HashTable String Literal -> StateT Store m ()
addScope ht = do Store loc glob <- get
                 put $ Store (ht:loc) glob

newScope :: StateT Store IO ()
newScope = do Store loc glob <- get
              loc' <- liftIO $ (:loc) <$> HT.new
              put $ Store loc' glob

popScope :: StateT Store IO ()
popScope = do Store loc glob <- get
              case loc of (_:loc') -> put $ Store loc' glob
                          []       -> error "popScope: Already in global scope"

withScope :: Monad m 
          => HashTable String Literal 
          -> StateT Store m a
          -> StateT Store m a
withScope ht = local (\(Store loc glob) -> Store (ht:loc) glob)

makeVar :: String -> Literal -> StateT Store IO ()
makeVar name val = do Store loc glob <- get
                      case loc of 
                          x:xs -> liftIO (HT.lookup x name) >>=
                                  \case Just _  -> liftIO $ HT.insert x name val
                                        Nothing -> local (const $ Store xs glob)
                                                         (makeVar name val)
                          []   -> liftIO (HT.lookup glob name) >>=
                                  \case Just _  -> liftIO $ HT.insert glob name val
                                        Nothing -> error $ "Cannot find variable '" ++ name ++ "'"