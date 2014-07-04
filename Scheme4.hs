module Main () where

import Control.Monad.Error

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

data LispExpression = LispSymbol String |
                      LispKeyword String |
                      LispList [LispExpression] |
                      LispNumber Integer |
                      LispString String |
                      LispBool Bool |
                      LispError String |
                      LispNil
     deriving(Show)

symbols :: Parser Char
symbols = oneOf "!#$%&|*+/:<=>?@^_~"

lispDef :: LanguageDef ()
lispDef
  = emptyDef
  { P.commentLine    = ";"
  , P.identStart     = letter <|> symbols
  , P.identLetter    = letter <|> digit <|> symbols
  , P.reservedNames  = []
  , P.caseSensitive  = True
  }

lexer = P.makeTokenParser lispDef

whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
lexeme = P.lexeme lexer



parseTrue :: Parser LispExpression
parseTrue = do
  void $ try (string "true")
  return $ LispBool True

parseFalse :: Parser LispExpression
parseFalse = do
  void $ try (string "false")
  return $ LispBool False




parseString :: Parser LispExpression
parseString = do _ <- char '"'
                 x <- many (noneOf "\"")
                 _ <- char '"'
                 return $ LispString x

parseKeyword :: Parser LispExpression
parseKeyword = do _ <- char ':'
                  x <- many alphaNum
                  return $ LispString x


parseNumber :: Parser LispExpression
parseNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 (digit)
  if (length sign) > 1
    then pzero
    else return $ (LispNumber . read) $ sign ++ num


parseSymbol :: Parser LispExpression
parseSymbol = do
  first <- letter <|> symbols
  rest <- many (letter <|> digit <|> symbols)
  let symbol = first:rest
  return $ LispSymbol symbol

parseList :: Parser LispExpression
parseList = liftM LispList $ sepBy parseLispExpression whiteSpace

parseLispExpression :: Parser LispExpression
parseLispExpression = parseTrue <|>
                      parseFalse <|>
                      parseKeyword <|>
                      parseSymbol <|>
                      lexeme parseNumber <|>
                      parseString <|>
                      parens parseList


-- parse parseLispExpression "lisp" "(a 1 1)"
