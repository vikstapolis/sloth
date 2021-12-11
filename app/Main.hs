module Main where

import Evaluate.Block
import Evaluate.Statement
import Text.Megaparsec
import Parse.Parser
import Control.Monad.State (runStateT)
import Data.Text
import Control.Monad (void)
import System.Environment (getArgs)
import qualified Data.HashTable.IO as HT
import Syntax
import Evaluate.Internal
import Builtins


main :: IO ()
main = do
    initStore <- Store <$> HT.fromList []
                       <*> HT.fromList builtins
    [filename] <- getArgs
    prog <- readFile filename
    case runParser (many parseStatement <* eof) filename (pack prog) of
        Right ast -> void (runStateT (evalBlock ast) initStore)
        Left err -> putStrLn $ errorBundlePretty err

getFloat :: [Literal] -> IO Literal
getFloat [StringL msg] = fmap FloatL $ putStrLn msg >> readLn

{-
Store <$> HT.fromList []
                    <*> HT.fromList
                    [ ("print", FuncL ["toPrint"] [OutputS (LitE $ VarL "toPrint")])
                    , ("getInt", FuncL ["msg"] [InputS "Int" $ LitE (VarL "msg")])
                    , ("getFloat", FuncL ["msg"] [ReturnS $ ExternE [LitE (VarL "msg")] getFloat])
                    , ("getLine", FuncL ["msg"] [InputS "String" $ LitE (VarL "msg")])
                    , ("getBool", FuncL ["msg"] [InputS "Bool" $ LitE (VarL "msg")])
                    ]
-}