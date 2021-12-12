{-# LANGUAGE OverloadedStrings #-}

module Parse.Parser where

import Syntax
import Parse.Lexer
import qualified DataStructs.Vector as V

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

import qualified Control.Monad.Combinators.Expr as E
import Control.Monad (join, foldM, void, replicateM)

import Data.Function ((&))
import Data.List (genericReplicate)

parseStatement :: Parser Statement
parseStatement = choice
                    [ parseFunc
                    , parseIf
                    , parseWhile
                    , parseReturn <* lexeme (char ';')
                    , ExpressionS <$> (parseExpression <* lexeme (char ';'))
                    ]
    where
        parseFunc :: Parser Statement
        parseFunc = do
            keyword "func"
            name <- identifier
            args <- parens $ sepBy identifier (lexeme $ char ',')
            body <- curlies (many parseStatement)
            return $ ExpressionS (DeclarationE name (LitE $ FuncL args body))

        parseIf :: Parser Statement
        parseIf = IfS
              <$> (keyword "if" *> parseExpression)
              <*> curlies (many parseStatement)
              <*> optional (
                    try (string "el" *> parseIf)
                    <|> keyword "else"
                        *> curlies
                            (IfS (LitE $ BoolL True)
                                <$> many parseStatement
                                <*> return Nothing)
                        )

        parseWhile :: Parser Statement
        parseWhile = WhileS
                 <$> (keyword "while" *> parseExpression)
                 <*> curlies (many parseStatement)

        parseReturn :: Parser Statement
        parseReturn = ReturnS
                  <$> (keyword "return" *> parseExpression)

parseArithOperator :: Parser Operator
parseArithOperator = try . lexeme $ choice
                    [ PlusO     <$ "+"
                    , MinusO    <$ "-"
                    , MultiplyO <$ "*"
                    , DivideO   <$ "/"
                    , ModuloO   <$ "%"
                    ]


parseExpression :: Parser Expression
parseExpression = try (lexeme parseDeclaration)
              <|> try (lexeme parseAssignment)
              <|> E.makeExprParser parseTerm operatorTable
    where
        parseTerm = choice $ map try
                    [ parseCallAndIndexing
                    , parseLiteral
                    , parens parseExpression
                    , parens parseDeclaration
                    , parens parseAssignment
                    ]

        operatorTable :: [[E.Operator Parser Expression]]
        operatorTable =
            [ [ binary "*" (OpE MultiplyO)
              , binary "/" (OpE DivideO)
              , binary "%" (OpE ModuloO)
              ]
            , [ binary "+" (OpE PlusO)
              , binary "-" (OpE MinusO)
              ]
            , [ E.InfixL $ (\x y ->
                  OpE OrO
                    (OpE LesserO x y)
                    (OpE EqualO x y)
                ) <$ try (lexeme "<=")
              , E.InfixL $ (\x y ->
                  OpE OrO
                    (OpE GreaterO x y)
                    (OpE EqualO x y)
                ) <$ try (lexeme ">=")
              , binary "<" (OpE LesserO)
              , binary ">" (OpE GreaterO)
              ]
            , [ binary "==" (OpE EqualO)
              , binary "!=" (OpE NotEqualO)
              ]
            , [ binary "&&" (OpE AndO)
              ]
            , [ binary "||" (OpE OrO)
              ]
            ]

        binary :: Text -> (Expression -> Expression -> Expression)
               -> E.Operator Parser Expression
        binary name f = E.InfixL (f <$ symbol name)

        parseCallAndIndexing :: Parser Expression
        parseCallAndIndexing = try $ do
            exp <- parens parseExpression
               <|> LitE . VarL <$> identifier
            callOrIdxs <- many $ 
                                flip CallE <$> parseArgList 
                            <|> flip (OpE IndexO) <$> squares parseExpression
            return $ foldl (&) exp callOrIdxs

        parseDeclaration :: Parser Expression
        parseDeclaration = DeclarationE
                       <$> (keyword "var" *> identifier <* lexeme (char '='))
                       <*> parseExpression

        parseAssignment :: Parser Expression
        parseAssignment = try (AssignmentE
                      <$> identifier
                      <*> optional parseArithOperator
                      <*> (lexeme (char '=') *> parseExpression))

                      <|> do var <- identifier
                             idxs <- many $ squares parseExpression
                             op  <- optional parseArithOperator
                             lexeme (char '=')
                             val <- parseExpression
                             return $ AssignmentE
                                        var
                                        op
                                        (ExternE ([LitE $ VarL var, val] ++ idxs)
                                            updateList)


updateList :: [Literal] -> IO Literal
updateList (l:val:idxs)
    = updateList' l val idxs >> return l
    where
        updateList' (ListL l) val [IntL i]      = void $ V.write (fromIntegral i) val l
        updateList' (ListL l) val (IntL i:idxs) = do l' <- l V.! fromIntegral i
                                                     updateList' l' val idxs

parseLiteral :: Parser Expression
parseLiteral = choice
                [ try $
                  LitE . FloatL  <$> float
                , LitE . IntL    <$> integer
                , LitE . BoolL   <$> (True <$ keyword "true"
                                        <|> False <$ keyword "false")
                , LitE . StringL <$> stringLiteral
                , LitE . VarL    <$> identifier
                , LitE           <$> parseLambda
                , parseList
                ]
    where
        parseLambda :: Parser Literal
        parseLambda = do lexeme $ char '|'
                         args <- fmap (:[]) identifier <|>
                                 parens (sepBy identifier (lexeme $ char ','))
                         lexeme "=>"
                         body <- fmap ((:[]) . ReturnS) parseExpression <|>
                                 curlies (many parseStatement)
                         lexeme $ char '|'
                         return $ FuncL args body

        parseList :: Parser Expression
        parseList = try (do lexeme $ char '['
                            lit <- parseLiteral
                            lexeme $ char ':'
                            len <- parseExpression
                            lexeme $ char ']'
                            return $ ExternE [len, lit] makeUniformList)

                <|> do expList <- squares $ sepBy parseExpression (lexeme $ char ',')
                       return $ ExternE expList makeList

makeList :: [Literal] -> IO Literal
makeList xs = ListL <$> V.fromList xs

makeUniformList :: [Literal] -> IO Literal
makeUniformList [IntL i, ListL v]
    = fmap ListL $ V.fromList =<< replicateM (fromIntegral i) (ListL <$> V.clone v)
makeUniformList [IntL i, e] = ListL <$> V.fromList (genericReplicate i e)

parseArgList :: Parser [Expression]
parseArgList = parens $ sepBy parseExpression (lexeme $ char ',')