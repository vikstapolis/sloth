{-# LANGUAGE LambdaCase #-}

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
                         IntL j   -> return $ IntL (i + j)
                         FloatL f -> return $ FloatL (fromIntegral i + f)
                         _        -> invalidOperandError '+' x y
        FloatL f  -> case yL of
                         IntL j   -> return $ FloatL (fromIntegral j + f)
                         FloatL g -> return $ FloatL (f + g)
                         _        -> invalidOperandError '+' x y
        StringL s -> case yL of
                         StringL t -> return $ StringL (s ++ t)
                         _         -> invalidOperandError '+' x y
        ListL l   -> case yL of
                         ListL m   -> liftIO $ ListL <$> m `V.concat` l
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
        ListL l -> case yL of
                       IntL i -> liftIO $ l V.! fromIntegral i
                       _        -> invalidOperandError 'i' x y

        _        -> invalidOperandError 'i' x y

evalExp (CallE name argExps)  = do
    Store loc glob <- get
    maybeFunc <- liftIO $
                    (<|>) 
                    <$> HT.lookup loc name
                    <*> HT.lookup glob name

    case maybeFunc of
        Nothing -> error $ "Couldn't find function '" ++ name ++ "'"

        Just (FuncL formalParams body) -> do
            args <- mapM evalExp argExps
            if length args /= length formalParams
            then error "Actual and formal parameters differ in length"
            else do
                args <- liftIO $ HT.fromList (zip formalParams args)
                retVal <- local (\store -> store {localVars=args})
                          (evalBlock body)
                return $ fromMaybe VoidL retVal


invalidOperandError :: Char -> Expression -> Expression -> a
invalidOperandError op x y = error $ "Invalid operands for ("
                          ++ op : ") operator: " ++ show x ++ "," ++ show y