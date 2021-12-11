module Main where

import Evaluate.Statement ( evalStmt )
import Text.Megaparsec
    ( runParser, errorBundlePretty, many, MonadParsec(eof) )
import Parse.Parser ( parseStatement )

import Evaluate.Internal ( Store(Store) )
import qualified Data.HashTable.IO as HT
import Builtins ( builtins )

import qualified Data.Text.IO as T

import Control.Monad.State (runStateT)
import Control.Monad (void)
import System.Environment (getArgs)


main :: IO ()
main = do
    initStore <- Store [] <$> HT.fromList builtins
    [filename] <- getArgs
    prog <- T.readFile filename
    case runParser (many parseStatement <* eof) filename prog of
        Right ast -> void (runStateT (mapM_ evalStmt ast) initStore)
        Left err  -> putStrLn $ errorBundlePretty err