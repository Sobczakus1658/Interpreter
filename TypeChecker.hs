module TypeChecker (checkProgram) where

import Debug.Trace
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import AbsGramatyka
import Data.Maybe
import AbsGramatyka (Type'(Bool), ArgType' (VArgType, PArgType), ArgType)
import Control.Exception.Base (TypeError)
import GHC.IO.Encoding (argvEncoding)

type Loc  = Int

type Env  = M.Map String TypeVal

type Exceptions = Exceptions' BNFC'Position
data Exceptions' a = VariableDoesNotExist String a | InvalidType TypeVal a |  NumberOfArgumentsDoesNotMatch String a | PointerExpected a deriving Show

data ArgTypeVal = Value TypeVal | Pointer TypeVal deriving (Eq, Show)

data TypeVal = BoolV | IntV | StringV | VoidV | FunV [ArgTypeVal] TypeVal deriving (Eq, Show)

type REI = ReaderT Env (ExceptT Exceptions IO)
-- u mnie musi się kompilować wszystko , a nie tylko do returna. Niepoprawnosc funkcji sprawdzam w runProgram.


checkEval :: TypeVal -> TypeVal -> BNFC'Position -> REI Bool
checkEval a b position =  do
    -- liftIO $ putStrLn ("TYPY :" ++ show a ++ show b )
    if a == b then return True else throwError (InvalidType a position)

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

createType:: Arg -> TypeVal
createType (VArg _ t _) = castToMyType t
createType (PArg {}) = IntV

createTypes :: [Arg] -> [ArgTypeVal]
createTypes = map castArgToMyArgType

getIdent:: Arg -> Ident
getIdent (VArg _ _ ident) = ident
getIdent (PArg _ _ ident) = ident

checkArgs :: [ArgTypeVal] -> [Expr] -> Ident -> BNFC'Position-> REI Bool
checkArgs [] [] _  _= return True
checkArgs (t:types) (e:exprs) ident position = do
    case t of
        Pointer _ -> do
            case e of
                EVar _ _ -> do
                    expr <- eval e
                    checkEval (castMyArgTypeToMyType t) expr position
                    checkArgs types exprs ident position
                _ -> throwError (PointerExpected position)
        Value _ -> do
            expr <- eval e
            checkEval (castMyArgTypeToMyType t) expr position
            checkArgs types exprs ident position
checkArgs _ _ (Ident name) position = throwError (NumberOfArgumentsDoesNotMatch name position)

evalExprs :: [Expr] -> REI TypeVal
evalExprs [] = return BoolV
evalExprs (expr: exprs) = do
    eval expr
    evalExprs exprs

eval :: Expr -> REI TypeVal
eval (ELitInt _ int) = do
    return IntV

eval (ELitTrue _) = do
    return BoolV

eval (ELitFalse _) = do
    return BoolV

eval (EString _ _) = do
    return StringV

eval (Neg _ expr) = do
    return IntV

eval (EMul position e1 op e2) = do
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 IntV position
    checkEval n1 IntV position
    return IntV

eval (EAdd position e1 op e2) = do
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 IntV position
    checkEval n1 IntV position
    env <- ask
    return IntV

eval (ERel position e1 op e2) = do
    env <- ask
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 IntV position
    checkEval n1 IntV position
    return BoolV

eval (EAnd position e1 e2) = do
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 IntV position
    checkEval n1 IntV position
    return BoolV

eval (EOr position e1 e2) = do
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 IntV position
    checkEval n1 IntV position
    return BoolV

eval (EVar position name) = do
    env <- ask
    getType name position


eval (ELam _ args t (SBlock _ stmts)) = do
    env <- ask
    let argTypes = createTypes args
        fun = FunV argTypes (castToMyType t)
    updatedEnv <- addVariables env args
    local (const updatedEnv) (checkStatemets stmts)
    -- liftIO $ putStrLn $ "Lambda type: " ++ show t
    return fun


eval (EApp position ident args) =  do
    env <- ask
    case ident of
        Ident "print" -> evalExprs args
        _ -> do
            FunV argTypes returnType <- getType ident position
            checkArgs argTypes args ident position
            return returnType


getType:: Ident -> BNFC'Position ->  REI TypeVal
getType (Ident x)  position = do
    env <- ask
    case M.lookup x env of
        Just l -> return l
        _ -> throwError (VariableDoesNotExist x position)

check :: Stmt -> REI Env
check (BStmt _ (SBlock _ s))  = do
    env <- ask
    checkStatemets s

check (Decl _ t []) = do
    ask


check (Decl position t ((Init _ (Ident x) s) : xs)) = do
    evalType <- eval s
    checkEval evalType (castToMyType t) position
    env <- ask
    let updatedEnv = M.insert x (castToMyType t) env
    local (const updatedEnv) (check (Decl position t xs))

check (Ass position s e) = do
    env <- ask
    correctType <- getType s position
    evalType <- eval e
    checkEval evalType correctType position
    ask

check (VRet _) = do
    ask

check (Cond _ e (SBlock _ s)) = do
    evalType <- eval e
    checkStatemets s

check (CondElse position e (SBlock _ s1) (SBlock _ s2)) = do
    evalType <- eval e
    checkEval evalType  BoolV position
    checkStatemets s1
    checkStatemets s2

check (While position e s) = do
    evalType <- eval e
    checkEval evalType  BoolV position
    check s
    ask

check (SExp _ e) = do
    eval e
    ask

check (Ret _ e) = do
    x <- eval e
    ask

check (FunExp _ (PFnDef _ t (Ident ident) args (SBlock _ stmts))) = do
    env <- ask
    let argTypes = createTypes args
        fun = FunV argTypes (castToMyType t)
        updatedEnv = M.insert ident fun env
    -- liftIO $ putStrLn (show types) 
    updatedEnv' <- addVariables updatedEnv args
    -- liftIO $ putStrLn ("siemaneczko" ++ show updatedEnv' ++ "\n\n " ++ show stmts) 
    local (const updatedEnv') (checkStatemets stmts)
    --zastnowić się którego enva zwracać
    return updatedEnv

addVariables :: Env -> [Arg] -> REI Env
addVariables = foldM addVariable

addVariable :: Env -> Arg -> REI Env
addVariable env (VArg _ t (Ident x)) = do
    return $ M.insert x (castToMyType t) env
addVariable env (PArg _ t (Ident x)) = do
    return $ M.insert x (castToMyType t) env



checkStatemets :: [Stmt] -> REI Env
checkStatemets [] = do
    ask
checkStatemets (s:xs) = do
    env <- check s
    -- liftIO $ putStrLn $ "checkStatements env: " ++ show env ++ "\nstmts: " ++ show (s:xs) ++ "\n"
    local (const env) (checkStatemets xs)

checkProgram :: Program -> IO (Either Exceptions ())
checkProgram (SProgram _ stmts) = runExceptT $ do
    void $ runReaderT (checkStatemets stmts) env0
  where
    env0 = M.empty
