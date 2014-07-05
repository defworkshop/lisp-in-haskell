module Main where

import           Control.Applicative
import qualified Data.List as L
import qualified Data.Map as M
import           Text.ParserCombinators.Parsec hiding ((<$>), (<|>), many, optional)
import           System.IO (hFlush, stdout)
import           System.Environment (getArgs)

type Env = M.Map String LispVal

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Boolean Bool
             | Nil
             deriving Show

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", foldr (\(Number n) (Number acc) -> Number (acc+n)) (Number 0))
             , ("-", L.foldl1 (\(Number acc) (Number n) -> Number (acc-n)))
             , ("<", lispCmp "<" (<))
             , (">", lispCmp ">" (>))
             ]

lispCmp :: String -> (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
lispCmp nm _ []                           = error $ "'" ++ nm ++ "' needs at least one argument"
lispCmp nm cmp (Number n : [])              = Boolean True
lispCmp nm cmp (Number n : Number m : rest) = if cmp n m then lispCmp nm cmp (Number m : rest) else Boolean False


eval :: Env -> LispVal -> (Env, LispVal)

eval env (Number n)                       = (env, Number n)
eval env (Boolean b)                      = (env, Boolean b)

eval env (Atom a)                         =
     case M.lookup a env of
          Just x -> (env, x)
          Nothing -> error $ "Variable " ++ a ++ " not found in " ++ show env

eval env (List [Atom "let", List bindings, body]) =
  let env'  = M.fromList $ map (\(List [Atom x, y]) -> (x, snd $ eval env y)) bindings
      env'' = M.union env' env
  in (env, snd $ eval env'' body)

eval env (List [Atom "setf",   (Atom var), expr]) = (M.insert var (snd $ eval env expr) env, Nil)

eval env (List [Atom "lambda", (List params), ast]) = undefined

eval env (List (Atom a: args))            =
     case lookup a primitives of
          Nothing -> error $ "Unknown form: " ++ a
          Just f -> (env, f $ map (snd . eval env) args)

eval env (List (Atom a: args))            =
     case lookup a primitives of
          Nothing -> error $ "Unknown form: " ++ a
          Just f -> (env, f $ map (snd . eval env) args)

eval env val                              = error $ "Unknown form: " ++ pprint val


run :: Env -> [LispVal] -> (Env, LispVal)
run env = L.foldl' (\(env',_) lexpr -> eval env' lexpr) (env, Nil)


----------------------------------------------------------------------
-- Pretty Printer
----------------------------------------------------------------------
pprint :: LispVal -> String
pprint (Atom a)    = a
pprint (Number n)  = show n
pprint (Boolean b) = if b then "#t" else "#f"
pprint (List l)    = "(" ++ L.intercalate " " (map pprint l) ++ ")"
pprint (Boolean b) = "nil"

showProgram = putStrLn . L.intercalate "\n" . map pprint



----------------------------------------------------------------------
-- The extended parser
----------------------------------------------------------------------
symbol :: Parser Char
symbol = oneOf "!#$%&|*+/:<=>?@^_~-"

whiteSpace :: Parser String
whiteSpace = many (oneOf " \t\n\r")

parseTrue :: Parser LispVal
parseTrue = string "#t" *> pure (Boolean True)

parseFalse :: Parser LispVal
parseFalse = string "#f" *> pure (Boolean False)

parseNil :: Parser LispVal
parseNil = string "nil" *> pure Nil

parseAtom :: Parser LispVal
parseAtom = Atom <$> ((:) <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol))

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = string "(" *> (List <$> sepBy parseLispVal spaces) <* string ")"

parseLispVal :: Parser LispVal
parseLispVal =  parseTrue
            <|> parseFalse
            <|> parseNil
            <|> parseAtom
            <|> parseNumber
            <|> parseList

parseProgram :: Parser [LispVal]
parseProgram = endBy parseLispVal (spaces *> optional eof)

programParser :: String -> [LispVal]
programParser s = case parse parseProgram "lisp" s of
  Right v -> v
  Left err -> error $ show err

exprParser :: String -> LispVal
exprParser s = case parse parseLispVal "lisp" s of
  Right v -> v
  Left err -> error $ show err


----------------------------------------------------------------------
-- Repl and Interpreter
----------------------------------------------------------------------
repl :: Env -> IO ()
repl env = do
  putStr "lisp> "
  hFlush stdout
  input <- getLine
  if input == "quit"
    then return ()
    else do let (env', val) = eval env' $ exprParser input
            putStrLn $ pprint $ val
            repl env'

main :: IO ()
main = do
  args <- getArgs
  if null args
    then repl M.empty
    else do prog <- readFile $ head args
            putStrLn prog
            putStrLn $ pprint $ snd $ run M.empty (programParser prog)


----------------------------------------------------------------------
-- Exercise 3-1: Extend 'eval' with an 'if' form which takes three
-- arguments: a condition, a then clause and an else clause
----------------------------------------------------------------------


----------------------------------------------------------------------
-- Exercise 3-2: Introduce a new constructor 'Fun' in LispVal which
-- holds user defined functions. A function needs its arguments, the
-- environment it was defined in and a body.
----------------------------------------------------------------------


----------------------------------------------------------------------
-- Exercise 3-3: Use the parts defined in 3-1 and 3-2 to define a
-- function that calculates the n-th Fibonacci number and test it.
----------------------------------------------------------------------
