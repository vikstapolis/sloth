{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexer where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

stringLiteral :: Parser String
stringLiteral = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme (L.signed (return ()) L.decimal)

float :: Parser Float
float = lexeme (L.signed (return ()) L.float)

anyKeyword :: Parser String
anyKeyword = fmap T.unpack . lexeme . try $ choice
            [ string "if",
              try $ string "else", -- So that "elif" matches
              string "elif",
              string "while",
              string "var",
              try $ string "func", -- So that "false" matches
              string "return",
              string "true",
              string "false"
            ] <* notFollowedBy alphaNumChar

keyword :: Text -> Parser String
keyword s = T.unpack <$> try (lexeme $ string s <* notFollowedBy alphaNumChar)


identifier :: Parser String
identifier = try (lexeme $ 
                    notFollowedBy anyKeyword *>
                    ((:) <$> letterChar <*> many alphaNumChar)
                )
         <?> "identifier"

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

curlies :: Parser a -> Parser a
curlies = between (lexeme $ char '{') (lexeme $ char '}')

squares :: Parser a -> Parser a
squares = between (lexeme $ char '[') (lexeme $ char ']')