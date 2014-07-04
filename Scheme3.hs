module Main where

import qualified Data.Map as M
import qualified Data.List as L

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Boolean Bool
             | Fun [String] Env LispVal
             deriving Show

prettyPrint :: LispVal -> String
prettyPrint (Atom str) = str
prettyPrint (List xs) = "(" ++ L.intercalate " " (map prettyPrint xs) ++ ")"
prettyPrint (Number i) = show i
prettyPrint (Boolean False) = "#f"
prettyPrint (Boolean True) = "#t"
prettyPrint (Fun args _ ast) =
  "(lambda (" ++ L.intercalate " " args ++ ") " ++ prettyPrint ast ++ ")"


type Env = M.Map String LispVal

a    = Atom "a"
b    = Atom "b"
c    = Atom "c"
f    = Atom "f"
g    = Atom "g"

x    = Atom "x"
y    = Atom "y"
z    = Atom "z"

_main = Atom "main"
_if = Atom "if"

_0   = Number 0
_1   = Number 1
_2   = Number 2
_3   = Number 3
_4   = Number 4
_5   = Number 5
_6   = Number 6
_7   = Number 7
_8   = Number 8
_9   = Number 9
_10   = Number 10


plus = Atom "+"
minus = Atom "-"
less = Atom "<"
_let = Atom "let"

setf = Atom "setf"
defn = Atom "defn"
quote = Atom "quote"

env :: Env
env = M.fromList [("a", _3), ("c", _5)]

lexpr1 = List [plus, _5, _5]

lexpr2 = List [Atom "+", Atom "x", Number 5]

lexpr3 = List [Atom "<", List [Atom "+", Number 1, Number 7],
                         List [Atom "+", Number 1, Number 5], List [Atom undefined]
                         ]

lexpr4 = List [Atom "list", Atom "a", Atom "b", Atom "c"]

lexpr5 = List [Atom "list", lexpr1, Atom "a", Atom "b", Atom "c"]

lexpr6 = List [plus, a, _3]

lexpr7 = List [_let, List [List [a,_1], List [b,_2]], List [plus, a, b, c]]

-- Task 1: Write a pretty-printer for LispVal which prints Lisp expressions
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", foldr (\(Number n) (Number acc) -> Number (acc+n)) (Number 0))
             ,("-", L.foldl1 (\(Number acc) (Number n) -> Number (acc-n)))
             ,("<", \(arg:args) -> fst $ foldr
                                   (\(Number n) ((Boolean result), (Number lst)) ->
                            if result && lst < n
                            then (Boolean True, Number n)
                            else (Boolean False, Number lst))
                   (Boolean True, arg) args)
             ,("list", List)
             ]



evalFunc :: String -> Env -> [LispVal] -> LispVal
evalFunc fun env args =
  case M.lookup fun env of
       Nothing -> error $ "unknown function " ++ fun ++ " in " ++ show env
       Just (Fun params closure ast) ->
         let env' = M.fromList $ zip params args 
             env'' = M.unions [env', closure, env]
         in eval env'' ast



eval :: Env -> LispVal -> LispVal

eval _ (List (Atom "setf" : _)) = error "setf: not at top level"
eval _ (List (Atom "defn" : _)) = error "defn: not at top level"


eval env (Number n)                       = Number n
eval env (Boolean b)                      = Boolean b

eval env (List (Atom "quote": arg : []))  = arg

eval env (List (Atom "if" : cond : th : el : [])) =
     case eval env cond of
          Boolean True -> eval env th
          Boolean False -> eval env el
          _ -> error $ "condition in if-expression must evaluate to boolean"
 
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
          Nothing -> evalFunc a env (map (eval env) args)
          Just f -> f $ map (eval env) args





exec :: Env -> LispVal -> Env
exec env (List [Atom "setf", (Atom var), expr]) =
  M.insert var (eval env expr) env
exec env (List [Atom "defn", (List (Atom f : params)), ast]) =
  M.insert f (Fun (map (\(Atom str) -> str) params) env ast) env

{-

(defn (f x y) (+ x y))

(setf main (f 1 2))

-}


fun = List [defn, List [f, x, y], List [plus, x, y]]

_fib = Atom "fib"
fib = List [defn, List [_fib, x], List [_if, cond, _1, List [plus, List [_fib, n1], List [_fib, n2]]]]
  where n1 = List [minus, x, _1]
        n2 = List [minus, x, _2]
        cond = List [less, x, _3]

m = List [setf, x, List [quote, minus, _5, _1]]

_20 = Number 20

program :: [LispVal]
program = [List [setf, a, _8], fib, List [setf, _main, List [ _fib, a]]]

program2 :: [LispVal]
program2 = [List [setf, a, _2], m]

showProgram = putStrLn . L.intercalate "\n" . map prettyPrint

initEnv = M.empty

run :: [LispVal] -> Maybe LispVal
run = M.lookup "main" . L.foldl' exec initEnv
