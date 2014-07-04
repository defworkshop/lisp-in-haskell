module Main where

data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Boolean Bool
             deriving Show


a    = Atom "a"
_5   = Number 5
plus = Atom "+"
less = Atom "<"

lexpr1 = List [plus, _5, _5]

lexpr2 = List [Atom "+", Atom "x", Number 5]

lexpr3 = List [Atom "<", List [Atom "+", Number 1, Number 7],
                         List [Atom "+", Number 1, Number 5], List [Atom undefined]
                         ]

lexpr4 = List [Atom "list", Atom "a", Atom "b", Atom "c"]

lexpr5 = List [Atom "list", lexpr1, Atom "a", Atom "b", Atom "c"]

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

eval :: LispVal -> LispVal
eval (Atom a)                         = Atom a
eval (Number n)                       = Number n
eval (Boolean b)                      = Boolean b
eval (List (Atom "quote": arg : []))  = arg
eval (List (Atom a: args))            = case lookup a primitives of
  Nothing -> error $ "Unknown form: " ++ a
  Just f -> f $ map eval args

-- Task: Parser bauen

-- Typesynonym für env definieren mit (Data.Map String LispVal)

-- Eval anpassen, so dass Variablen interpretiert werden können
