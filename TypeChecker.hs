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

-- data TypeT = BoolT Bool | IntT Integer | StringT String | VoidT | FunT FunBody deriving Show

type FunBody = ([Stmt], Env, [Type], Type)

type REI = ReaderT Env (ExceptT Exceptions IO) 
-- u mnie musi się kompilować wszystko , a nie tylko do returna. Niepoprawnosc funkcji sprawdzam w runProgram.


checkEval :: Type -> Type -> REI Bool
checkEval a b = if a == b then return True else throwError (InvalidType a)

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
    -- liftIO $ putStrLn ("Sprawdzamy: " ++ show types ++ "  " ++show exprs ) 
    expr <- eval e
    checkEval t expr
    checkArgs types exprs ident
checkArgs _ _ (Ident name) = throwError (NumberOfArgumentsDoesNotMatch name)

evalExprs :: [Expr] -> REI Type
-- evalExprs a = do 
--     liftIO $ putStrLn ("Sprawdzamy auaua \n: " ++ show a ) 
--     return Bool
evalExprs [] = return Bool
evalExprs (expr: exprs) = do 
    env <-ask
    -- liftIO $ putStrLn ("Sprawdzamy kurwa \n: " ++ show env ) 
    eval expr
    -- liftIO $ putStrLn ("Sprawdzamy auaua \n: " ++ show exprs ) 
    evalExprs exprs

eval :: Expr -> REI Type
eval (ELitInt int) = do 
    -- liftIO $ putStrLn ("Int: " ++ show int ) 
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
    -- liftIO $ putStrLn ("Relacja " ++ show env ++ "\n") 
    return Int

eval (ERel e1 op e2) = do 
    env <- ask
    -- liftIO $ putStrLn ("Relacja " ++ show env ++ "\n") 
    n1 <- eval e1
    -- liftIO $ putStrLn ("funkcja: ") 
    n2 <- eval e2
    -- liftIO $ putStrLn ("funkcja: ") 
    checkEval n1 Int
    checkEval n1 Int
    -- liftIO $ putStrLn ("funkcja: ") 
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
    -- liftIO $ putStrLn ("Var: " ++ show env ) 
    getType name


eval (ELam args t (SBlock stmts)) = do 
    env <- ask
    let types = createTypes args
        fun = Fun types t 
    local (const env) (checkStatemets stmts)
    return t

eval (EApp ident args) =  do 
    env <- ask
    -- liftIO $ putStrLn ("funkcja: " ++ show env ) 
    case ident of 
        Ident "print" -> evalExprs args
        _ -> do
            Fun argTypes returnType <- getType ident 
            checkArgs argTypes args ident
            return returnType


getType:: Ident -> REI Type
getType (Ident x) = do
    liftIO $ putStrLn ("dzien dobry: " ++ show x) 
    env <- ask
    -- Drukujemy klucze i wartości w mapie env
    liftIO $ putStrLn "Wartość env:"
    liftIO $ mapM_ (\(k, v) -> putStrLn $ show k ++ ": " ++ show v) (M.toList env)

    -- liftIO $ putStrLn ("dzien dobry: " ++ show env) 
    -- liftIO $ putStrLn ("papa") 
    case M.lookup x env of
        Just l -> return l
        _ -> throwError (VariableDoesNotExist x)

checkBlock :: [Stmt] -> Env -> REI Env
checkBlock [] env = do
    return env

checkBlock (stmt:stmts) env = do
    newEnv <- check stmt
    let newEnv = M.union newEnv env
    local (const newEnv) $ do checkBlock stmts newEnv

check :: Stmt -> REI Env
check (BStmt (SBlock s))  = do
    env <- ask
    finalEnv <- checkBlock s env
    return finalEnv

check (Decl t []) = do
    env <- ask
    -- liftIO $ putStrLn ("Wszedlem\n\n: " ++ show env)
    return env


check (Decl t ((Init (Ident x) s) : xs)) = do 
    -- liftIO $ putStrLn ("Wszedlem\n\n: " ++ show x)
    -- let ok =  isAvailableType t
    -- case ok of
    --     False -> throwError (DivByZero t)
    --     _ -> do
    evalType <- eval s
    checkEval evalType t
    env <- ask
    let updatedEnv = M.insert x t env
    local (const updatedEnv) (check (Decl t xs))

check (Ass s e) = do 
    env <- ask
    -- liftIO $ putStrLn ("Przypisanie\n\n: " ++ show env)
    correctType <- getType s
    evalType <- eval e
    checkEval evalType correctType
    env <- ask
    return env

check (VRet) = do 
    env <- ask
    return env

check (Cond e (SBlock s)) = do 
    -- liftIO $ putStrLn ("if: \n\n" ++ show e ) 
    evalType <- eval e
    -- checkEval evalType  Bool
    -- liftIO $ putStrLn ("if: \n\n" ++ show e ) 
    checkStatemets s

check (CondElse e (SBlock s1) (SBlock s2)) = do 
    evalType <- eval e
    checkEval evalType  Bool
    checkStatemets s1
    checkStatemets s2

check (While e s) = do
    -- liftIO $ putStrLn ("While: " ++ show s ) 
    evalType <- eval e
    checkEval evalType  Bool
    check s
    env <- ask
    return env

check (SExp e) = do 
    env <- ask
    -- liftIO $ putStrLn ("sex" ++ show env ) 
    eval e
    env <- ask
    -- liftIO $ putStrLn ("Sexp: " ++ show env ) 
    return env

check (Ret e) = do
    x <- eval e
    env <- ask
    return env

check (FunExp (PFnDef t (Ident ident) args (SBlock stmts))) = do

    env <- ask
    -- liftIO $ putStrLn ("Ident: " ++ show ident)
    -- liftIO $ putStrLn ("Env\n: " ++ show env)
    let types = createTypes args
        fun = Fun types t 
        updatedEnv = M.insert ident fun env
    updatedEnv' <- addVariables updatedEnv args types
    -- liftIO $ putStrLn ("Type\n\n: " ++ show updatedEnv)
    local (const updatedEnv') (checkStatemets stmts)
    return updatedEnv
--
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
    -- liftIO $ putStrLn ("zawieszeVarnie: " ++ show env ) 
    env <- check s
    -- liftIO $ putStrLn ("zawieszeVarnie: " ++ show env ) 
    local (const env) (checkStatemets xs)

checkProgram :: Program -> IO (Either Exceptions ()) 
checkProgram (SProgram stmts) = runExceptT $ do
    void $ runReaderT (checkStatemets stmts) env0
  where
    env0 = M.empty 
