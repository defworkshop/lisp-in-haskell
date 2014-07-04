module Main where

----------------------------------------------------------------------
-- The initial datatype to represent Lisp expressions in Haskell
----------------------------------------------------------------------
data LispVal = Atom String
             | List [LispVal]
             | Number Integer
             | Boolean Bool
             deriving Show


----------------------------------------------------------------------
-- We can write expressions such as the following in Haskell:
-- (+ 5 10)
----------------------------------------------------------------------
lexpr1 = List [Atom "+", Number 5, Number 10]


----------------------------------------------------------------------
-- (+ x 5)
----------------------------------------------------------------------
lexpr2 = List [Atom "+", Atom "x", Number 5]


----------------------------------------------------------------------
-- (< (+ 1 7) (+ 1 5))
----------------------------------------------------------------------
lexpr3 = List [Atom "<", List [Atom "+", Number 1, Number 7],
                         List [Atom "+", Number 1, Number 5]]


----------------------------------------------------------------------
-- (list a b c)
----------------------------------------------------------------------
lexpr4 = List [Atom "list", Atom "a", Atom "b", Atom "c"]


----------------------------------------------------------------------
-- Tip: when experimenting with such expressions in Haskell, it can be
-- convenient to define some short hands
----------------------------------------------------------------------
a       = Atom "a"
b       = Atom "b"
c       = Atom "c"
_1      = Number 1
_2      = Number 2
_3      = Number 3
_4      = Number 4
_5      = Number 5
plus    = Atom "+"
minus   = Atom "-"
less    = Atom "<"
greater = Atom ">"
list    = Atom "list"

----------------------------------------------------------------------
-- (< (+ 1 1 1) (- 5 1) 5)
----------------------------------------------------------------------
lexpr5 = List [less, List [plus, _1, _1, _1], List [minus, _5, _3], _5]


----------------------------------------------------------------------
-- Let's define a simple evaluator for our language
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
             ,("<", lispLess)
             ]

lispLess :: [LispVal] -> LispVal
lispLess []                           = error "'<' needs at least one argument"
lispLess (Number n : [])              = Boolean True
lispLess (Number n : Number m : rest) = if n < m
                                        then lispLess (Number m : rest)
                                        else Boolean False

----------------------------------------------------------------------
-- Exercise 1: Implement the primitive '-'. Make sure the following
-- expressions eval to the correct results:
----------------------------------------------------------------------
lexpr_ex1_1 = List [minus, _5, _3]                                    -- (Number 2)
lexpr_ex1_2 = List [minus, _3, _1, _1, _1]                            -- (Number 0)
lexpr_ex1_3 = List [minus, List [minus, _5, _1], List [plus, _3, _2]] -- Number -1


----------------------------------------------------------------------
-- Exercise 2: Implement the primitive '>'. This should be very
-- similar to '<'. Make sure the following expressions eval to the
-- correct results. Can you abstract the common parts out?
----------------------------------------------------------------------
lexpr_ex2_1 = List [greater, _5, _1]     -- (Boolean True)
lexpr_ex2_2 = List [greater, _2, _4]     -- (Boolean False)
lexpr_ex2_3 = List [greater, _3, _2, _1] -- (Boolean True)
lexpr_ex2_4 = List [greater, _5, _2, _2] -- (Boolean False)

----------------------------------------------------------------------
-- Exercise 3: Implement a pretty printer for LispVal which takes the
-- Haskell expressions back to their Lisp string representation.
----------------------------------------------------------------------
