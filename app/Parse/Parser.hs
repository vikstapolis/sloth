{-# LANGUAGE OverloadedStrings #-}

module Parse.Parser where

import Syntax
import Parse.Lexer

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Function ((&))

import qualified Control.Monad.Combinators.Expr as E
import Control.Monad (join)

import qualified DataStructs.Vector as V
import Control.Monad.IO.Class (liftIO)
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

parseOperator :: Parser Operator
parseOperator = try . lexeme $ choice
                    [ PlusO     <$ "+"
                    , MinusO    <$ "-"
                    , MultiplyO <$ "*"
                    , DivideO   <$ "/"
                    , EqualO    <$ "=="
                    , GreaterO  <$ ">"
                    , LesserO   <$ "<"
                    , ModuloO   <$ "%"
                    ]


parseExpression :: Parser Expression
parseExpression = try (lexeme parseDeclaration)
              <|> try (lexeme parseAssignment)
              <|> E.makeExprParser parseTerm operatorTable
    where
        parseTerm = choice $ map try
                    [ parseIndexing
                    , parseCallExp
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
            , [ binary "<" (OpE LesserO)
              , binary ">" (OpE GreaterO)
              ]
            , [ binary "==" (OpE EqualO)
              ]
            ]

        binary :: Text -> (Expression -> Expression -> Expression)
               -> E.Operator Parser Expression
        binary name f = E.InfixL (f <$ symbol name)

        parseCallExp :: Parser Expression
        parseCallExp = CallE
                   <$> identifier
                   <*> parens (sepBy parseExpression (lexeme ","))

        parseDeclaration :: Parser Expression
        parseDeclaration = DeclarationE
                       <$> (keyword "var" *> identifier <* lexeme (char '='))
                       <*> parseExpression

        parseAssignment :: Parser Expression
        parseAssignment = try (AssignmentE
                      <$> identifier
                      <*> optional parseOperator
                      <*> (lexeme (char '=') *> parseExpression))

                      <|> do var <- identifier
                             idx <- squares parseExpression
                             op  <- optional parseOperator
                             lexeme (char '=')
                             val <- parseExpression
                             return $ AssignmentE
                                        var 
                                        op
                                        (ExternE [LitE $ VarL var, idx, val]
                                            updateList)

        parseIndexing :: Parser Expression
        parseIndexing = try $ do
                lst <- try parseCallExp <|> parens parseExpression <|> (LitE . VarL <$> identifier)
                idx <- squares parseExpression
                return $ OpE IndexO lst idx

updateList :: [Literal] -> IO Literal
updateList [ListL l, IntL i, val]
    = V.write (fromIntegral i) val l >> return (ListL l)

parseLiteral :: Parser Expression
parseLiteral = choice
                [ try $ 
                  LitE . FloatL  <$> float
                , LitE . IntL    <$> integer
                , LitE . BoolL   <$> ((True <$ keyword "true")
                                        <|> (False <$ keyword "false"))
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
makeUniformList [IntL i, e] = ListL <$> V.fromList (genericReplicate i e)