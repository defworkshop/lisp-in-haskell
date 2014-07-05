module Main where

import           Control.Applicative
import qualified Data.List as L
import qualified Data.Map as M
import           Text.ParserCombinators.Parsec hiding ((<$>), (<|>), many)
import           System.IO (hFlush, stdout)

type Env = M.Map String LispVal

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Boolean Bool
             deriving Show


eval :: Env -> LispVal -> LispVal

eval _ (List (Atom "setf" : _)) = error "setf: not at top level"
eval _ (List (Atom "defn" : _)) = error "defn: not at top level"

eval env (Number n)                       = Number n
eval env (Boolean b)                      = Boolean b

eval env (Atom a)                         =
     case M.lookup a env of
          Just x -> x
          Nothing -> error $ "Variable " ++ a ++ " not found in " ++ show env

eval env (List [Atom "let", List bindings, body]) =
  let env' = M.fromList $ map (\(List [Atom x, y]) -> (x, eval env y)) bindings
      env'' = M.union env' env
  in eval env'' body
eval env (List (Atom a: args))            =
     case lookup a primitives of
          Nothing -> error $ "Unknown form: " ++ a
          Just f -> f $ map (eval env) args

exec :: Env -> LispVal -> Env
exec env (List [Atom "setf", (Atom var), expr]) =
  M.insert var (eval env expr) env
exec env (List [Atom "defn", (List (Atom f : params)), ast]) = undefined

showProgram = putStrLn . L.intercalate "\n" . map pprint

initEnv = M.empty

run :: [LispVal] -> Maybe LispVal
run = M.lookup "main" . L.foldl' exec initEnv


----------------------------------------------------------------------
-- Stuff from Scheme2.hs
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

pprint :: LispVal -> String
pprint (Atom a)    = a
pprint (Number n)  = show n
pprint (Boolean b) = if b then "#t" else "#f"
pprint (List l)    = "(" ++ L.intercalate " " (map pprint l) ++ ")"

main :: IO ()
main = do putStr "lisp> "
          hFlush stdout
          input <- getLine
          if input == "quit"
            then return ()
            else do putStrLn $ pprint $ eval M.empty $ parser input
                    main


----------------------------------------------------------------------
-- Exercise 3-1: Extend 'eval' with an 'if' form which takes three
-- arguments: a condition, a then clause and an else clause
----------------------------------------------------------------------


----------------------------------------------------------------------
-- Exercise 3-2: Introduce a new constructor 'Fun' in LispVal which
-- holds user defined functions. A function needs its arguments, the
-- environment it was defined in and a body
----------------------------------------------------------------------


----------------------------------------------------------------------
-- Exercise 3-3: Use the parts defined in 3-1 and 3-2 to define a
-- function that calculates the n-th Fibonacci number and test it.
----------------------------------------------------------------------
