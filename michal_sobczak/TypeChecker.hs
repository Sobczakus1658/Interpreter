{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module TypeChecker (checkProgram) where

import Debug.Trace
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad
import Control.Monad.Except
import AbsGramatyka
import Data.Maybe
import Control.Exception.Base (TypeError)
import GHC.IO.Encoding (argvEncoding)
import ExceptionsAndTypes
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception (throw)

checkEval :: TypeVal -> TypeVal -> BNFC'Position -> REI Bool
checkEval a b position =  do
    if a == b then return True else throwError (InvalidType a position)

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

eval (Not _ expr) = do
    return BoolV

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
    checkEval n1 BoolV position
    checkEval n1 BoolV position
    return BoolV

eval (EOr position e1 e2) = do
    n1 <- eval e1
    n2 <- eval e2
    checkEval n1 BoolV position
    checkEval n1 BoolV position
    return BoolV

eval (EVar position name) = do
    env <- ask
    getType name position


eval (ELam _ args t (SBlock _ stmts)) = do
    env <- ask
    let argTypes = createTypes args
        fun = FunV argTypes (castToMyType t)
    updatedEnv <- addVariables env args
    local (const updatedEnv) (checkStatemets stmts (Just (castToMyType t)))
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

check :: Stmt -> RetCheck -> REI (TypeEnv, RetCheck)
check (Empty _) _ = do
    env <- ask
    return (env, Nothing)

check (BStmt _ (SBlock _ s)) t  = do
    env <- ask
    checkStatemets s t

check (Decl _ t []) _ = do
    env <- ask
    return (env, Nothing)

check (Decl position t ((Init _ (Ident x) s) : xs)) retType = do
    let myType = castToMyType t
    case myType of 
        VoidV -> throwError (NotAllowedType myType position)
        _ -> do
            evalType <- eval s
            checkEval evalType (castToMyType t) position
            env <- ask
            let updatedEnv = M.insert x (castToMyType t) env
            local (const updatedEnv) (check (Decl position t xs) retType)

check (Ass position s e) _ = do
    env <- ask
    correctType <- getType s position
    evalType <- eval e
    checkEval evalType correctType position
    env <- ask
    return (env, Nothing)

check (VRet position) retType = do
    case retType of
        Just VoidV -> do
            env <- ask
            return (env, Just VoidV)
        Just t -> throwError (ReturnType t VoidV position)
        Nothing -> do
            env <- ask
            return (env, Just VoidV)

check (Cond position e (SBlock _ s)) retType = do
    evalType <- eval e
    checkEval evalType  BoolV position
    checkStatemets s retType
    env <- ask
    return (env, Nothing)

check (CondElse position e (SBlock _ s1) (SBlock _ s2)) retType = do
    evalType <- eval e
    checkEval evalType  BoolV position
    (_, ret1) <- checkStatemets s1 retType
    (_, ret2) <- checkStatemets s2 retType
    env <- ask
    if ret1 == ret2 then return (env, ret1) else return (env, Nothing)

check (While position e s) retType = do
    evalType <- eval e
    checkEval evalType  BoolV position
    check s retType
    env <- ask
    return (env, Nothing)

check (SExp _ e) _ = do
    ret <- eval e
    env <- ask
    return (env, Nothing)

check (Ret position e) retType = do
    ret <- eval e
    case retType of
        Just t -> do
            if t == ret then do
                env <- ask
                return (env, Just ret)
            else do
                throwError (ReturnType t ret position)
        Nothing -> do
            env <- ask
            return (env, Just ret)


check (FunExp _ (PFnDef position t (Ident ident) args (SBlock _ stmts))) _ = do
    env <- ask
    let retType = castToMyType t
    let argTypes = createTypes args
        fun = FunV argTypes (castToMyType t)
        updatedEnv = M.insert ident fun env
    updatedEnv' <- addVariables updatedEnv args
    (_, ret)<-local (const updatedEnv') (checkStatemets stmts (Just retType))
    case ret of
        Nothing -> do
            if retType == VoidV
                then return (updatedEnv, Nothing)
            else do
                throwError (NoReturn position)
        Just a -> return (updatedEnv, Nothing)


addVariables :: TypeEnv -> [Arg] -> REI TypeEnv
addVariables = foldM addVariable

addVariable :: TypeEnv -> Arg -> REI TypeEnv
addVariable env (VArg _ t (Ident x)) = do
    return $ M.insert x (castToMyType t) env
addVariable env (PArg _ t (Ident x)) = do
    return $ M.insert x (castToMyType t) env

checkStatemets :: [Stmt] -> RetCheck -> REI (TypeEnv, RetCheck)
checkStatemets [] _ = do
    env <- ask
    return (env, Nothing)
checkStatemets (s:xs) retType = do
    (env, retType1) <- check s retType
    (_, retType2) <- local (const env) (checkStatemets xs retType)
    case retType1 of
        Just a -> return (env, Just a)
        Nothing -> return (env, retType2)

checkProgram :: Program -> IO (Either ExceptionsChecker ())
checkProgram (SProgram _ stmts) = runExceptT $ do
    void $ runReaderT (checkStatemets stmts Nothing) env0
  where
    env0 = M.empty
