module Language.WorkshopScheme.Interpreter where

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)             = return val
eval env val@(Number _)             = return val
eval env val@(Bool _)               = return val
eval env (Atom id)                  = getVar env id
eval env (List [Atom "quote", val]) = evalQuoted env val
eval env (List (Atom "defmacro" : List (Atom var : params) : body)) = makeMacro Nothing env params body >>= defineVar env var
eval env (List [Atom "if", pred, conseq, alt]) = do result <- eval env pred
                                                    case result of
                                                      Bool False -> eval env alt
                                                      otherwise  -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define": List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  if isFunc func
    then mapM (eval env) args >>= apply func
    else apply func args
eval env badForm                    = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalQuoted :: Env -> LispVal -> IOThrowsError LispVal
evalQuoted env (List [Atom "unquote", val]) = eval env val
evalQuoted env (List xs) = mapM (evalQuoted env) xs >>= return . List
evalQuoted env (DottedList xs x) = do newXs <- mapM (evalQuoted env) xs
                                      newX  <- evalQuoted env x
                                      return $ DottedList newXs newX
evalQuoted env x = return x

isFunc :: LispVal -> Bool
isFunc (Func _ _ _ _)    = True
isFunc (PrimitiveFunc _) = True
isFunc (IOFunc _)        = True
isFunc otherwise         = False

-- evalMacro :: Env -> LispVal -> [LispVal] -> IOThrowsError
-- evalMacro env (Macro params varargs body closure) args =
--   (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>=
          bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env
apply (Macro params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>=
          bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env
apply badForm _ = throwError $ BadSpecialForm "Can't call apply on" badForm

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func:args)       = apply func args
