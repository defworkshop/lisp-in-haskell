module Main where

import qualified Data.Map as M

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Boolean Bool
             deriving Show

type Env = M.Map String LispVal

a    = Atom "a"
b    = Atom "b"
c    = Atom "c"
_1   = Number 1
_2   = Number 2
_3   = Number 3
_4   = Number 4
_5   = Number 5
plus = Atom "+"
less = Atom "<"
_let = Atom "let"

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
             ,("-", foldr (\(Number n) (Number acc) -> Number (acc-n)) (Number 0))
             ,("<", \(arg:args) -> fst $ foldr
                                   (\(Number n) ((Boolean result), (Number lst)) ->
                            if result && lst < n
                            then (Boolean True, Number n)
                            else (Boolean False, Number lst))
                   (Boolean True, arg) args)
             ,("list", List)
             ]

eval :: Env -> LispVal -> LispVal
eval env (Atom a)                         = case M.lookup a env of Just x -> x
eval env (Number n)                       = Number n
eval env (Boolean b)                      = Boolean b
eval env (List (Atom "quote": arg : []))  = arg
eval env (List [Atom "let", List bindings, body]) =
  let env' = M.fromList $ map (\(List [Atom x, y]) -> (x, eval env y)) bindings
      env'' = M.union env' env
  in eval env'' body
eval env (List (Atom a: args))            = case lookup a primitives of
  Nothing -> error $ "Unknown form: " ++ a
  Just f -> f $ map (eval env) args

-- Fehlerbehandlung

-- Überprüfe ob die bindings in let die richtige Struktur haben

-- Funktionsdefinitionen
