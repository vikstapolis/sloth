{-# LANGUAGE TupleSections #-}
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
                                 then evalBlock block
                                 else maybe (return Nothing)
                                            evalBlock
                                            maybeElse

evalStmt (WhileS cond block)  = do
    cond' <- evalExp cond
    case cond' of BoolL b -> if b
                                 then evalBlock block
                                   >> evalStmt (WhileS cond block)
                                 else return Nothing

evalStmt (DeclarationS name exp) = do
    Store loc glob <- get
    maybeVal <- liftIO $
            (<|>)
            <$> HT.lookup loc  name
            <*> HT.lookup glob name

    case maybeVal of Nothing -> do
                                val <- evalExp exp
                                isGlobalScope <- liftIO $ null <$> HT.toList loc

                                if isGlobalScope
                                    then liftIO $ HT.insert glob name val
                                    else liftIO $ HT.insert loc  name val

                                return Nothing

                     Just _  -> error $ "Variable '" ++ name ++ "' already defined"

evalStmt (AssignmentS name maybeOp exp) = do
    Store loc glob <- get
    maybeVal <- liftIO $
            (<|>)
            <$> (fmap (False,) <$> HT.lookup loc  name)
            <*> (fmap (True,)  <$> HT.lookup glob name)

    case maybeVal of
        Just (isGlobal,oldVal) -> do
                val <- case maybeOp of
                           Just op -> evalExp $ OpE op (LitE oldVal) exp
                           Nothing -> evalExp exp

                if isGlobal
                    then liftIO $ HT.insert glob name val
                    else liftIO $ HT.insert loc  name val

                return Nothing

        Nothing -> error $ "Cannot find variable '" ++ name ++ "'"

-- evalStmt (Return expr) = do
--     retVal <- evalExp expr
--     Store loc glob <- get
--     liftIO $ HT.insert glob "_" retVal

evalStmt (ReturnS expr) = Just <$> evalExp expr

evalStmt (OutputS exp) = evalExp exp >>= liftIO . print >> return Nothing
evalStmt (InputS inpType msgExp) = do
    msg <- evalExp msgExp
    liftIO $ print msg
    input <- liftIO getLine

    case inpType of "String" -> return $ Just (StringL input)
                    "Int"    -> return $ Just (IntL (read input))
                    "Float"  -> return $ Just (FloatL (read input))
                    "Bool"   -> return $ Just (BoolL (read input))
                    _        -> error $ "Cannot input type '" ++ inpType ++ "'"

-- program :: Statement
-- program = IfS (OpE EqualO
--                 (LitE (IntL 5))
--                 (OpE PlusO (LitE (IntL 2)) (LitE (IntL 3))))
--               [OutputS (LitE $ StringL "Equal")]
--               (Just [OutputS (LitE $ StringL "Not Equal")])

program = [
        DeclarationS "inputInt"
            (LitE $
                FuncL ["msg"]
                      [InputS "Int" $ LitE (VarL "msg")]
                ),
        DeclarationS "add"
            (LitE $
                FuncL ["x","y"]
                      [ReturnS $ OpE PlusO
                          (LitE $ VarL "x")
                          (LitE $ VarL "y")
                          ]
                ),
        DeclarationS "a"
            (CallE "inputInt" [LitE $ StringL "Enter a number"]),
        DeclarationS "b"
            (CallE "inputInt" [LitE $ StringL "Enter another number"]),
        OutputS (CallE "add" [LitE $ VarL "a",LitE $ VarL "b"])
    ]

initStore :: IO Store
initStore = join Store <$> HT.fromList []