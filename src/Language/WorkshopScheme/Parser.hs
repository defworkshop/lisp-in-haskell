module Language.WorkshopScheme.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Language.WorkshopScheme.AST

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

spaces :: Parser ()
spaces = skipMany1 space

escapedChar :: Parser Char
escapedChar = oneOf "\\" >>= \x -> oneOf "\"nrt\\" >>= \y ->
  return $ case y of
    '"' -> '"'
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    '\\' -> '\\'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChar <|> (noneOf "\""))
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                 "#t"      -> Bool True
                 "#f"      -> Bool False
                 otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumber' :: Parser LispVal
parseNumber' = do
  x <- many1 digit
  return $ (Number . read) x

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= \x -> return $ (Number . read) x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  char ',' >> parseExpr >>= \x -> return $ List [Atom "unquote", x]

parseChar :: Parser LispVal
parseChar = char '\\' >> (letter <|> digit <|> symbol) >>= return . Char

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> parseUnquoted
            <|> do char '('
                   x <- (try parseList) <|> parseDottedList
                   char ')'
                   return x
