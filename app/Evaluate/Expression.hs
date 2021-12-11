{-# LANGUAGE TupleSections #-}

module Evaluate.Expression (
    evalExp,
) where

import Syntax

import DataStructs.Vector (Vector)
import qualified DataStructs.Vector as V

import Control.Monad.Trans.State (StateT, get)

import Evaluate.Internal
import qualified Data.HashTable.IO as HT

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import {-# SOURCE #-} Evaluate.Block (evalBlock)
import Data.Fixed (mod')

evalExp :: Expression
        -> StateT Store IO Literal

evalExp (LitE (VarL name))  = do
    Store loc glob <- get
    maybeVal <- liftIO $
            (<|>)
            <$> HT.lookup loc  name
            <*> HT.lookup glob name
    case maybeVal of Just lit -> return lit
                     Nothing  -> error $ "Cannot find variable '" ++ name ++ "'"

evalExp (LitE lit)  = return lit

evalExp (OpE PlusO x y)  = do
    xL <- evalExp x
    yL <- evalExp y

    case xL of
        IntL i    -> case yL of
                         IntL j    -> return $ IntL (i + j)
                         FloatL f  -> return $ FloatL (fromIntegral i + f)
                         StringL s -> return $ StringL (show i ++ s)
                         _        -> invalidOperandError '+' x y
        FloatL f  -> case yL of
                         IntL j   -> return $ FloatL (fromIntegral j + f)
                         FloatL g -> return $ FloatL (f + g)
                         StringL s -> return $ StringL (show f ++ s)
                         _        -> invalidOperandError '+' x y
        StringL s -> case yL of
                         StringL t -> return $ StringL (s ++ t)
                         IntL i    -> return $ StringL (s ++ show i)
                         FloatL f  -> return $ StringL (s ++ show f)
        ListL l   -> case yL of
                         ListL m   -> liftIO $ ListL <$> l `V.concat` m
        _         -> invalidOperandError '-' x y

evalExp (OpE MinusO x y)  = do
    xL <- evalExp x
    yL <- evalExp y

    case xL of
        IntL i   -> case yL of
                        IntL j   -> return $ IntL (i - j)
                        FloatL f -> return $ FloatL (fromIntegral i - f)
                        _        -> invalidOperandError '-' x y
        FloatL f -> case yL of
                        IntL j   -> return $ FloatL (fromIntegral j - f)
                        FloatL g -> return $ FloatL (f - g)
                        _        -> invalidOperandError '-' x y
        _        -> invalidOperandError '-' x y

evalExp (OpE MultiplyO x y)  = do
    xL <- evalExp x
    yL <- evalExp y

    case xL of
        IntL i   -> case yL of
                        IntL j   -> return $ IntL (i * j)
                        FloatL f -> return $ FloatL (fromIntegral i * f)
                        _        -> invalidOperandError '*' x y
        FloatL f -> case yL of
                        IntL j   -> return $ FloatL (fromIntegral j * f)
                        FloatL g -> return $ FloatL (f * g)
                        _        -> invalidOperandError '*' x y
        _        -> invalidOperandError '*' x y

evalExp (OpE DivideO x y)  = do
    xL <- evalExp x
    yL <- evalExp y

    case xL of
        IntL i   -> case yL of
                        IntL j   -> return $ FloatL (fromIntegral i / fromIntegral j)
                        FloatL f -> return $ FloatL (fromIntegral i / f)
                        _        -> invalidOperandError '/' x y
        FloatL f -> case yL of
                        IntL j   -> return $ FloatL (fromIntegral j / f)
                        FloatL g -> return $ FloatL (f / g)
                        _        -> invalidOperandError '/' x y
        _        -> invalidOperandError '/' x y

evalExp (OpE ModuloO x y) = do
    xL <- evalExp x
    yL <- evalExp y

    case xL of
        IntL i   -> case yL of
                        IntL j   -> return $ IntL (i `mod'` j)
                        FloatL f -> return $ FloatL (fromIntegral i `mod'` f)
                        _        -> invalidOperandError '/' x y
        FloatL f -> case yL of
                        IntL j   -> return $ FloatL (fromIntegral j `mod'` f)
                        FloatL g -> return $ FloatL (f `mod'` g)
                        _        -> invalidOperandError '/' x y
        _        -> invalidOperandError '/' x y


evalExp (OpE EqualO x y)  = do
    xL <- evalExp x
    yL <- evalExp y
    return $ BoolL (xL == yL)

evalExp (OpE LesserO x y)  = do
    xL <- evalExp x
    yL <- evalExp y
    return $ BoolL (xL < yL)

evalExp (OpE GreaterO x y)  = do
    xL <- evalExp x
    yL <- evalExp y
    return $ BoolL (xL > yL)

evalExp (OpE IndexO x y)  = do
    xL <- evalExp x
    yL <- evalExp y

    case xL of
        ListL l   -> case yL of
                        IntL i -> liftIO $ l V.! fromIntegral i
                        _        -> invalidOperandError 'i' x y
        StringL s -> case yL of
                        IntL i -> return $ StringL [s !! fromIntegral i]

        _        -> invalidOperandError 'i' x y

evalExp (CallE exp args) = do
    func <- evalExp exp
    case func of 
        FuncL params body -> do
            Store loc glob <- get;
            args <- mapM evalExp args
            if length args /= length params
            then error "Actual and formal parameters differ in length"
            else do
                args  <- liftIO $ HT.fromList (zip params args)

                -- To ensure that inside the function,
                -- we can find out that we are inside a function
                liftIO $ HT.insert args "_" VoidL

                retVal <- local (\store -> store {localVars=args})
                          (evalBlock body)
                return $ fromMaybe VoidL retVal

-- evalExp (CallE name argExps)  = do
--     Store loc glob <- get
--     maybeFunc <- liftIO $
--                     (<|>)
--                     <$> HT.lookup loc name
--                     <*> HT.lookup glob name

--     case maybeFunc of
--         Nothing -> error $ "Couldn't find function '" ++ name ++ "'"

--         Just (FuncL formalParams body) -> do
--             args <- mapM evalExp argExps
--             if length args /= length formalParams
--             then error "Actual and formal parameters differ in length"
--             else do
--                 args  <- liftIO $ HT.fromList (zip formalParams args)
--                 -- To ensure that inside the function, we can find out that we are inside a function
--                 liftIO $ HT.insert args "_" VoidL
--                 retVal <- local (\store -> store {localVars=args})
--                           (evalBlock body)
--                 return $ fromMaybe VoidL retVal

evalExp (DeclarationE name exp) = do
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

                                return val

                     Just _  -> error $ "Variable '" ++ name ++ "' already defined"

evalExp (AssignmentE name maybeOp exp) = do
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

                return val

        Nothing -> error $ "Cannot find variable '" ++ name ++ "'"

evalExp (ExternE args f) = mapM evalExp args >>= liftIO . f


invalidOperandError :: Char -> Expression -> Expression -> a
invalidOperandError op x y = error $ "Invalid operands for ("
                          ++ op : ") operator: " ++ show x ++ "," ++ show y