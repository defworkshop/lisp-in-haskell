module Main () where

import Control.Monad.Error

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

data LispVal = Atom String
     | LispList [LispVal]
     | Number Integer
     | String String
     | Boolean Bool
     | LispNil
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



parseTrue :: Parser LispVal
parseTrue = do
  void $ try (string "true")
  return $ Boolean True

parseFalse :: Parser LispVal
parseFalse = do
  void $ try (string "false")
  return $ Boolean False




parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (noneOf "\"")
                 _ <- char '"'
                 return $ String x

parseKeyword :: Parser LispVal
parseKeyword = do _ <- char ':'
                  x <- many alphaNum
                  return $ String x


parseNumber :: Parser LispVal
parseNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 (digit)
  if (length sign) > 1
    then pzero
    else return $ (Number . read) $ sign ++ num


parseSymbol :: Parser LispVal
parseSymbol = do
  first <- letter <|> symbols
  rest <- many (letter <|> digit <|> symbols)
  let symbol = first:rest
  return $ Atom symbol

parseList :: Parser LispVal
parseList = liftM LispList $ sepBy parseLispVal whiteSpace

parseLispVal :: Parser LispVal
parseLispVal = parseTrue <|>
                      parseFalse <|>
                      parseKeyword <|>
                      parseSymbol <|>
                      lexeme parseNumber <|>
                      parseString <|>
                      parens parseList

-- parse parseLispVal "lisp" "(a 1 1)"
-- parse parseLispVal "lisp" "(a \"asd\" true 1)"
