module Language.WorkshopScheme.AST where

import qualified Data.List as L
import           System.IO
import           Data.IORef (IORef)
import           Control.Monad.Error
import           Text.ParserCombinators.Parsec (ParseError)


-- FIXME: Having this in the AST seems *wrong*, but don't want to
-- change for now.
type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String),
                     body :: [LispVal], closure :: Env}
             | Macro {params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ (unwordsList xs) ++ ")"
showVal (DottedList xs x) = "(" ++ (unwordsList xs) ++ " . " ++ (showVal x) ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Macro {params = args, vararg = varargs, body = body, closure = env}) =
  "(macro (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ")\n" ++ L.intercalate "\n" (map showVal body)
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _)   = "<IO port>"

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispVal where
  show = showVal

instance Show LispError where
  show = showError

-- instance Error LispError where
--   noMsg  = Default "An error has occured"
--   strMsg = Default
