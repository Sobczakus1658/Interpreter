{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ExceptionsAndTypes where
    
import qualified Data.Map as M
import Control.Monad.Except
import AbsGramatyka
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- TypeChecker and ExceptionsAndTypes
data ArgTypeVal = Value TypeVal | Pointer TypeVal deriving (Eq, Show)
data TypeVal = BoolV | IntV | StringV | VoidV | FunV [ArgTypeVal] TypeVal deriving (Eq, Show)

--RunProgram
type Loc  = Int
type Env  = M.Map String Loc
type Store = M.Map Loc MemVal
type RetVal = Maybe MemVal
type FunBody = ([Stmt], Env, [Arg], Type)
data MemVal = VBool Bool | VInt Integer | VString String | VVoid | VFun FunBody deriving Show


type RSEI a = ReaderT Env (StateT Store (ExceptT ExceptionsRunning  IO)) a

type ExceptionsRunning = ExceptionsRunning' BNFC'Position
data ExceptionsRunning' a = DivByZero a | ModByZero a 

--TypeChecker
type RetCheck = Maybe TypeVal
type TypeEnv  = M.Map String TypeVal
type REI = ReaderT TypeEnv (ExceptT ExceptionsChecker IO)

type ExceptionsChecker = ExceptionsChecker' BNFC'Position
data ExceptionsChecker' a = VariableDoesNotExist String a 
                            | InvalidType TypeVal a 
                            | NumberOfArgumentsDoesNotMatch String a 
                            | PointerExpected a 
                            | NotAllowedType TypeVal a
                            | NoReturn a
                            | ReturnType TypeVal TypeVal a
                            | DifferentTypesReturn TypeVal TypeVal a

view :: BNFC'Position -> String
view (Just (line, column)) = show line ++ " at column: " ++ show column ++ ".\n"
view Nothing = "unknown position.\n"
viewTypeArg :: ArgTypeVal -> String
viewTypeArg (Value a) = viewType a
viewTypeArg (Pointer a) = "&" ++ viewType a

viewTypeArgs :: [ArgTypeVal] -> String
viewTypeArgs = concatMap viewTypeArg

viewType :: TypeVal -> String
viewType IntV = "Int"
viewType BoolV = "Bool"
viewType StringV = "String"
viewType VoidV = "Void"
viewType (FunV args t) = "Fun [ " ++ viewTypeArgs args ++ " ] -> " ++ viewType t

instance Show ExceptionsRunning where
    show (DivByZero position) = "ERROR: Divide by zero, line : " ++ view position
    show (ModByZero position) = "ERROR: Modulo by zero, line : " ++ view position

instance Show ExceptionsChecker where
    show (VariableDoesNotExist variable position) = "ERROR: Variable " ++ show variable ++ " does not exist at line : " ++ view position
    show (InvalidType t position) = "ERROR: Invalid type " ++ viewType t ++ " at line : " ++ view position
    show (NumberOfArgumentsDoesNotMatch name position) = "ERROR: Function " ++ show name ++ " has not got correct number of arguments at line : " ++ view position
    show (PointerExpected position) = "ERROR: Expected Pointer at line: " ++ view position
    show (NotAllowedType t position) = "ERROR: Not allowed type " ++ viewType t ++ " at line : " ++ view position
    show (NoReturn position) = "ERROR: No return statement at line : " ++ view position
    show (ReturnType expected actual position) = "ERROR: Expected return type " ++ viewType expected ++ " but got " ++ viewType actual ++ " at line : " ++ view position
    show (DifferentTypesReturn t1 t2 position) = "ERROR: Return type " ++ viewType t1 ++ " does not fit " ++ viewType t2 ++ " at line : " ++ view position

castToMyType :: Type -> TypeVal
castToMyType (Int _) = IntV
castToMyType (Str _) = StringV
castToMyType (Bool _) = BoolV
castToMyType (Void _) = VoidV
castToMyType (Fun _ args t) = FunV (map castArgTypeToMyArgType args) (castToMyType t)

castArgToMyArgType :: Arg -> ArgTypeVal
castArgToMyArgType (VArg _ t _) = Value $ castToMyType t
castArgToMyArgType (PArg _ t _) = Pointer $ castToMyType t

castArgTypeToMyArgType :: ArgType -> ArgTypeVal
castArgTypeToMyArgType (VArgType _ t) = Value $ castToMyType t
castArgTypeToMyArgType (PArgType _ t) = Pointer $ castToMyType t

castMyArgTypeToMyType :: ArgTypeVal -> TypeVal
castMyArgTypeToMyType (Value t) = t
castMyArgTypeToMyType (Pointer t) = t

castMemValToMyType :: MemVal -> TypeVal
castMemValToMyType (VBool _) = BoolV
castMemValToMyType (VInt _) = IntV
castMemValToMyType (VString _) = StringV
castMemValToMyType VVoid = VoidV
castMemValToMyType (VFun (_, _, args, t)) = FunV (map castArgToMyArgType args) (castToMyType t)

castRetValToMyType :: RetVal -> TypeVal
castRetValToMyType (Just value) = castMemValToMyType value
castRetValToMyType Nothing = VoidV