module Main where

import           Control.Applicative
import qualified Data.List as L
import qualified Data.Map as M
import           Text.ParserCombinators.Parsec hiding ((<$>), (<|>), many)
import           System.IO (hFlush, stdout)

----------------------------------------------------------------------
-- Parsing the Lisp Expressions and writing a REPL
----------------------------------------------------------------------

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Boolean Bool
             deriving Show


----------------------------------------------------------------------
-- A simple parser definition using Parsec
----------------------------------------------------------------------
symbol :: Parser Char
symbol = oneOf "!#$%&|*+/:<=>?@^_~-"

parseTrue :: Parser LispVal
parseTrue = string "#t" *> pure (Boolean True)

parseFalse :: Parser LispVal
parseFalse = string "#f" *> pure (Boolean False)

parseAtom :: Parser LispVal
parseAtom = Atom <$> ((:) <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol))

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = string "(" *> (List <$> sepBy parseLispVal spaces) <* string ")"

parseLispVal :: Parser LispVal
parseLispVal =  parseTrue
            <|> parseFalse
            <|> parseAtom
            <|> parseNumber
            <|> parseList


parser :: String -> LispVal
parser s = case parse parseLispVal "lisp" s of
  Right v -> v
  Left err -> error $ show err


----------------------------------------------------------------------
-- eval and prmitives are like before
----------------------------------------------------------------------
eval :: LispVal -> LispVal
eval (Atom a)                         = Atom a
eval (Number n)                       = Number n
eval (Boolean b)                      = Boolean b
eval (List (Atom a: args))            = case lookup a primitives of
  Nothing -> error $ "Unknown form: " ++ a
  Just f -> f $ map eval args


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", foldr (\(Number n) (Number acc) -> Number (acc+n)) (Number 0))
             ,("-", L.foldl1 (\(Number acc) (Number n) -> Number (acc-n)))
             ,("<", lispCmp "<" (<))
             ,(">", lispCmp ">" (>))
             ]


lispCmp :: String -> (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
lispCmp nm _ []                           = error $ "'" ++ nm ++ "' needs at least one argument"
lispCmp nm cmp (Number n : [])              = Boolean True
lispCmp nm cmp (Number n : Number m : rest) = if cmp n m then lispCmp nm cmp (Number m : rest) else Boolean False


----------------------------------------------------------------------
-- the pretty print function from last time
----------------------------------------------------------------------
pprint :: LispVal -> String
pprint (Atom a)    = a
pprint (Number n)  = show n
pprint (Boolean b) = if b then "#t" else "#f"
pprint (List l)    = "(" ++ L.intercalate " " (map pprint l) ++ ")"


----------------------------------------------------------------------
-- our REPL
----------------------------------------------------------------------
main :: IO ()
main = do putStr "lisp> "
          hFlush stdout
          input <- getLine
          if input == "quit"
            then return ()
            else do putStrLn $ pprint $ eval $ parser input
                    main

----------------------------------------------------------------------
-- Exercise 2-1: Introduce a type definiton for 'Env' to represent an
-- environment using Data.Map
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Exercise 2-2: Extend 'eval' such that it takes an environment and
-- looks up variables within this environment if they are not found in
-- 'primitives'
----------------------------------------------------------------------
