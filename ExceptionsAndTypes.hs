module ExceptionsAndTypes where
import qualified Data.Map as M
import Control.Monad.Except
import AbsGramatyka
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

type Loc  = Int

type TypeEnv  = M.Map String TypeVal
type Env  = M.Map String Loc

data ArgTypeVal = Value TypeVal | Pointer TypeVal deriving (Eq, Show)
data TypeVal = BoolV | IntV | StringV | VoidV | FunV [ArgTypeVal] TypeVal deriving (Eq, Show)

type Store = M.Map Loc MemVal

data MemVal = VBool Bool | VInt Integer | VString String | VVoid | VFun FunBody deriving Show
type RetVal = Maybe MemVal

type FunBody = ([Stmt], Env, [Arg], Type)

type ExceptionsRunning = ExceptionsRunning' BNFC'Position
data ExceptionsRunning' a = DivByZero a| ModByZero a | ReturnTypeError a 

type ExceptionsChecker = ExceptionsChecker' BNFC'Position
data ExceptionsChecker' a = VariableDoesNotExist String a | InvalidType TypeVal a |  NumberOfArgumentsDoesNotMatch String a | PointerExpected a

type RSEI a = ReaderT Env (StateT Store (ExceptT ExceptionsRunning  IO)) a

type REI = ReaderT TypeEnv (ExceptT ExceptionsChecker IO)

view :: BNFC'Position -> String 
view (Just (line, column)) = show line ++ " at column: " ++ show column ++ ".\n"

instance Show ExceptionsRunning where
    show (DivByZero position) = "ERROR: Divide by zero, line : " ++ view position
    show (ModByZero position) = "ERROR: Modulo by zero, line : " ++ view position
    show (ReturnTypeError position) = "ERROR: return type does not fit, line : " ++ view position

instance Show ExceptionsChecker where
    show (VariableDoesNotExist variable position) = "ERROR: Variable " ++ show variable ++ " does not exist at line : " ++ view position
    show (InvalidType t position) = "ERROR: Invalid type " ++ show t ++ "at line : " ++ view position
    show (NumberOfArgumentsDoesNotMatch name position) = "ERROR: Function " ++ show name ++ " has not got correct number of arguments at line : " ++ view position
    show (PointerExpected position) = "ERROR: Expected Pointer at line: " ++ view position
