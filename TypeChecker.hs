module TypeChecker (checkProgram) where

import Debug.Trace
import qualified Data.Map as M 
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Except
import AbsGramatyka
import Data.Maybe

type Loc  = Int

type Env  = M.Map String Type 

data Exceptions = VariableDoesNotExist String | InvalidType Type |  NumberOfArgumentsDoesNotMatch String deriving Show 

type FunBody = ([Stmt], Env, [Type], Type)

type REI = ReaderT Env (ExceptT Exceptions IO) 
-- u mnie musi się kompilować wszystko , a nie tylko do returna. Niepoprawnosc funkcji sprawdzam w runProgram.


checkEval :: Type -> Type -> REI Bool
checkEval a b =  do 
    -- liftIO $ putStrLn ("TYPY :" ++ show a ++ show b )
    if a == b then return True else throwError (InvalidType a)

createType:: Arg -> Type
createType (VArg t _) = t
createType (PArg _ _) = Int

createTypes :: [Arg] -> [Type]
createTypes a = map createType a

getIdent:: Arg -> Ident
getIdent (VArg _ ident) = ident
getIdent (PArg _ ident) = ident

checkArgs :: [Type] -> [Expr] -> Ident -> REI Bool
checkArgs [] [] _ = return True
checkArgs (t:types) (e:exprs) ident = do 
    expr <- eval e
    checkEval t expr
    checkArgs types exprs ident
checkArgs _ _ (Ident name) = throwError (NumberOfArgumentsDoesNotMatch name)

evalExprs :: [Expr] -> REI Type
evalExprs [] = return Bool
evalExprs (expr: exprs) = do 
    eval expr
    evalExprs exprs

eval :: Expr -> REI Type
eval (ELitInt int) = do 
    return Int

eval (ELitTrue) = do 
    return Bool

eval (ELitFalse) = do 
    return Bool

eval (EString _) = do 
    return Str

eval (Neg expr) = do 
    return Int
    
eval (EMul e1 op e2) = do 
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 Int
    checkEval n1 Int
    return Int

eval (EAdd e1 op e2) = do 
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 Int
    checkEval n1 Int
    env <- ask
    return Int

eval (ERel e1 op e2) = do 
    env <- ask
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 Int
    checkEval n1 Int
    return Bool

eval (EAnd e1 e2) = do 
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 Int
    checkEval n1 Int
    return Bool

eval (EOr e1 e2) = do 
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 Int
    checkEval n1 Int
    return Bool

eval (EVar name) = do 
    env <- ask
    getType name


eval (ELam args t (SBlock stmts)) = do 
    env <- ask
    let types = createTypes args
        fun = Fun types t 
    updatedEnv <- addVariables env args types
    local (const updatedEnv) (checkStatemets stmts)
    -- liftIO $ putStrLn $ "Lambda type: " ++ show t
    return $ Fun types t


eval (EApp ident args) =  do 
    env <- ask
    case ident of 
        Ident "print" -> evalExprs args
        _ -> do
            Fun argTypes returnType <- getType ident 
            checkArgs argTypes args ident
            return returnType


getType:: Ident -> REI Type
getType (Ident x) = do
    env <- ask
    case M.lookup x env of
        Just l -> return l
        _ -> throwError (VariableDoesNotExist x)

check :: Stmt -> REI Env
check (BStmt (SBlock s))  = do
    env <- ask
    finalEnv <- checkStatemets s 
    return finalEnv

check (Decl t []) = do
    env <- ask
    return env


check (Decl t ((Init (Ident x) s) : xs)) = do 
    evalType <- eval s
    checkEval evalType t
    env <- ask
    let updatedEnv = M.insert x t env
    local (const updatedEnv) (check (Decl t xs))

check (Ass s e) = do 
    env <- ask
    correctType <- getType s
    evalType <- eval e
    checkEval evalType correctType
    env <- ask
    return env

check (VRet) = do 
    env <- ask
    return env

check (Cond e (SBlock s)) = do 
    evalType <- eval e
    checkStatemets s

check (CondElse e (SBlock s1) (SBlock s2)) = do 
    evalType <- eval e
    checkEval evalType  Bool
    checkStatemets s1
    checkStatemets s2

check (While e s) = do
    evalType <- eval e
    checkEval evalType  Bool
    check s
    env <- ask
    return env

check (SExp e) = do 
    eval e
    env <- ask
    return env

check (Ret e) = do
    x <- eval e
    env <- ask
    return env

check (FunExp (PFnDef t (Ident ident) args (SBlock stmts))) = do
    env <- ask
    let types = createTypes args
        fun = Fun types t 
        updatedEnv = M.insert ident fun env
    -- liftIO $ putStrLn (show types) 
    updatedEnv' <- addVariables updatedEnv args types
    -- liftIO $ putStrLn ("siemaneczko" ++ show updatedEnv' ++ "\n\n " ++ show stmts) 
    local (const updatedEnv') (checkStatemets stmts)
    --zastnowić się którego enva zwracać
    return updatedEnv

addVariables :: Env -> [Arg] -> [Type] -> REI Env
addVariables env [] [] = return env
addVariables env (arg:args) (t:types) = do 
    let (Ident x) = getIdent arg
        updatedEnv = M.insert x t env
    addVariables updatedEnv args types

checkStatemets :: [Stmt] -> REI Env
checkStatemets [] = do
    env <- ask
    return env
checkStatemets (s:xs) = do
    env <- check s
    -- liftIO $ putStrLn $ "checkStatements env: " ++ show env ++ "\nstmts: " ++ show (s:xs) ++ "\n"
    local (const env) (checkStatemets xs)

checkProgram :: Program -> IO (Either Exceptions ()) 
checkProgram (SProgram stmts) = runExceptT $ do
    void $ runReaderT (checkStatemets stmts) env0
  where
    env0 = M.empty 
