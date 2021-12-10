module Builtins where

import Syntax
import Control.Monad ((>=>))
import qualified DataStructs.Vector as V

builtins = [ makeBuiltin "print"    ["x"]   print_
           , makeBuiltin "println"  ["x"]   println_
           , makeBuiltin "getInt"   ["msg"] getInt_
           , makeBuiltin "getFloat" ["msg"] getFloat_
           , makeBuiltin "getLine"  ["msg"] getLine_
           , makeBuiltin "getWord"  ["msg"] getWord_
           , makeBuiltin "round"    ["num"] (return . round_)
           , makeBuiltin "len"      ["x"]   (return . len_)
           , ("max", FuncL ["list"] [
                   ReturnS $ ExternE
                       [LitE $ VarL "list"]
                       (\[ListL list] -> do
                           list' <- V.toList list
                           return $ max_ list')
               ])
           , ("min", FuncL ["list"] [
                   ReturnS $ ExternE
                       [LitE $ VarL "list"]
                       (\[ListL list] -> do
                           list' <- V.toList list
                           return $ min_ list')
               ])
           ]

makeBuiltin :: String 
            -> [String] 
            -> ([Literal] -> IO Literal) 
            -> (String, Literal)
makeBuiltin name args f = (name, FuncL args [
                                    ReturnS $ ExternE (
                                        map (LitE . VarL) args
                                    ) f
                                ])

print_ = (VoidL <$) . (showLit >=> putStr) . head

println_ = (VoidL <$) . (showLit >=> putStrLn) . head

getInt_ [StringL msg] = fmap IntL $ putStrLn msg >> readLn

getFloat_ [StringL msg] = fmap FloatL $ putStrLn msg >> readLn

getLine_ [StringL msg] = fmap StringL $ putStrLn msg >> getLine

getWord_ [StringL msg] = fmap (StringL . head . words) $
                            putStrLn msg >> getLine

len_ [StringL s] = IntL . fromIntegral $ length s
len_ [ListL l]   = IntL . fromIntegral $ V.length l
len_ _           = error "Cannot find length"

round_ [FloatL f] = IntL $ round f

max_ [] = error "Require arguments for max"
max_ [x]              = x
max_ (FloatL f : xs)  = case max_ xs of
                        FloatL g -> FloatL $ max f g
                        IntL   i -> FloatL $ max f (fromIntegral i)
                        _        -> error "Cannot compare float with this literal"
max_ (IntL i : xs)    = case max_ xs of
                        FloatL f -> FloatL $ max f (fromIntegral i)
                        IntL   j -> IntL   $ max i j
                        _        -> error "Cannot compare int with this literal"
max_ (StringL s : xs) = case max_ xs of
                        StringL t -> StringL $ max s t
                        _         -> error "Cannot compare string with this literal"
max_ _                = error "This literal is not comparable"

min_ [] = error "Require arguments for min"
min_ [x]              = x
min_ (FloatL f : xs)  = case min_ xs of
                        FloatL g -> FloatL $ min f g
                        IntL   i -> FloatL $ min f (fromIntegral i)
                        _        -> error "Cannot compare float with this literal"
min_ (IntL i : xs)    = case min_ xs of
                        FloatL f -> FloatL $ min f (fromIntegral i)
                        IntL   j -> IntL   $ min i j
                        _        -> error "Cannot compare int with this literal"
min_ (StringL s : xs) = case min_ xs of
                        StringL t -> StringL $ min s t
                        _         -> error "Cannot compare string with this literal"
min_ _                = error "This literal is not comparable"