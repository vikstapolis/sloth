{-# LANGUAGE LambdaCase #-}

module Evaluate.Statement where

import Syntax
import Evaluate.Internal
import {-# SOURCE #-} Evaluate.Block
import Evaluate.Expression

import Control.Monad.State (StateT, get, runStateT)

import qualified Data.HashTable.IO as HT

import Control.Monad.IO.Class (liftIO)
import Control.Monad ( when, void, join )
import Control.Applicative ((<|>))

evalStmt :: Statement
         -> StateT Store IO (Maybe Literal)
evalStmt (IfS cond block maybeElse)  = do
    cond' <- evalExp cond
    case cond' of BoolL b -> if b
                                 then newScope >> evalBlock block
                                 else maybe (return Nothing)
                                            (\x -> newScope >> evalStmt x)
                                            maybeElse

evalStmt (WhileS cond block)  = do
    cond' <- evalExp cond
    case cond' of BoolL b -> if b
                                 then newScope >> evalBlock block >>= \case
                                        Nothing -> evalStmt (WhileS cond block)
                                        Just l  -> return (Just l)
                                 else return Nothing


evalStmt (ReturnS expr) = isGlobalScope >>=
                          \case False -> Just <$> evalExp expr
                                True  -> error "Return statement outside a function"

evalStmt (OutputS exp) = evalExp exp >>= liftIO . printLit >> return Nothing

evalStmt (InputS inpType msgExp) = do
    msg <- evalExp msgExp
    liftIO $ printLit msg
    input <- liftIO getLine

    case inpType of "String" -> return $ Just (StringL input)
                    "Int"    -> return $ Just (IntL (read input))
                    "Float"  -> return $ Just (FloatL (read input))
                    "Bool"   -> return $ Just (BoolL (read input))
                    _        -> error $ "Cannot input type '" ++ inpType ++ "'"

evalStmt (ExpressionS exp) = evalExp exp >> return Nothing