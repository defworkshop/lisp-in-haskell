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
             | Fun [String] Env LispVal
             | Nil
             deriving Show

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", foldr (\(Number n) (Number acc) -> Number (acc+n)) (Number 0))
             , ("-", L.foldl1 (\(Number acc) (Number n) -> Number (acc-n)))
             , ("<", lispCmp "<" (<))
             , (">", lispCmp ">" (>))
             ]

lispCmp :: String -> (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
lispCmp nm cmp (Number n : [])              = Boolean True
lispCmp nm cmp (Number n : Number m : rest) = if cmp n m then lispCmp nm cmp (Number m : rest) else Boolean False
lispCmp nm _   []                           = error $ "'" ++ nm ++ "' needs at least one argument"
lispCmp nm _   (a:as)                       = error $ "'" ++ nm ++ "': not a number: " ++ pprint a


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

eval env (List [Atom "if", cond, then_, else_]) = case snd $ eval env cond of
  Nil           -> eval env else_
  Number 0      -> eval env else_
  Boolean False -> eval env else_
  List []       -> eval env else_
  _             -> eval env then_

eval env (List [Atom "setf",   (Atom var), expr]) = (M.insert var (snd $ eval env expr) env, Nil)

eval env (List [Atom "lambda", (List params), ast]) = (env, Fun (map (\(Atom str) -> str) params) env ast)

eval env (List (Atom a: args))            =
  let args' = map (snd . eval env) args
  in case lookup a primitives of
          Nothing -> case M.lookup a env of
            Just (Fun params closure ast) -> let env' = M.fromList $ zip params args'
                                                 env'' = M.unions [env', closure, env]
                                             in (env, snd $ eval env'' ast)
            Just _                        -> error $ "Not a function: " ++ a
            Nothing                       -> error $ "Unknown form: " ++ a
          Just f -> (env, f args')

eval env val                              = error $ "Unknown form: " ++ pprint val


run :: Env -> [LispVal] -> (Env, LispVal)
run env = L.foldl' (\(env',_) lexpr -> eval env' lexpr) (env, Nil)


----------------------------------------------------------------------
-- Pretty Printer
----------------------------------------------------------------------
pprint :: LispVal -> String
pprint (Atom a)         = a
pprint (Number n)       = show n
pprint (Boolean b)      = if b then "#t" else "#f"
pprint (List l)         = "(" ++ L.intercalate " " (map pprint l) ++ ")"
pprint (Nil)            = "nil"
pprint (Fun args _ ast) = "(lambda (" ++ L.intercalate " " args ++ ") " ++ pprint ast ++ ")"

showProgram = putStrLn . L.intercalate "\n" . map pprint



----------------------------------------------------------------------
-- The extended parser
----------------------------------------------------------------------
symbol :: Parser Char
symbol = oneOf "!#$%&|*+/:<=>?@^_~-"

parseTrue :: Parser LispVal
parseTrue = try (string "#t") *> pure (Boolean True)

parseFalse :: Parser LispVal
parseFalse = try (string "#f") *> pure (Boolean False)

parseNil :: Parser LispVal
parseNil = try (string "nil") *> pure Nil

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
    else do let (env', val) = eval env $ exprParser input
            putStrLn $ pprint $ val
            repl env'

main :: IO ()
main = do
  args <- getArgs
  if null args
    then repl M.empty
    else do prog <- readFile $ head args
            putStrLn $ pprint $ snd $ run M.empty (programParser prog)
