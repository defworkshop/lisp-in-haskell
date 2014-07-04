module Main where

import qualified Data.Map as M
import qualified Data.List as L

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
exec env (List [Atom "defn", (List (Atom f : params)), ast]) =
  M.insert f (Fun (map (\(Atom str) -> str) params) env ast) env

showProgram = putStrLn . L.intercalate "\n" . map prettyPrint

initEnv = M.empty

run :: [LispVal] -> Maybe LispVal
run = M.lookup "main" . L.foldl' exec initEnv


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
