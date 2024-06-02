{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module RunProgram (exec) where

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad
import AbsGramatyka
import Data.Maybe
import ExceptionsAndTypes

newloc :: Store -> Loc
newloc m =  if M.null m then 0 else let (i,w) = M.findMax m in i+1

newloc' :: RSEI Loc
newloc' = do
    st <- get
    let l = newloc st
    modify (M.insert l VVoid)
    return l

printValue :: [Expr] -> RSEI RetVal
printValue (x:xs) = do
    w <- eval x
    liftIO $ putStrLn (memValToString w)
    return Nothing
printValue [] = return Nothing

memValToString :: MemVal -> String
memValToString (VBool b) = if b then "True" else "False"
memValToString (VInt n) = show n
memValToString (VString s) = s
memValToString VVoid = "Void"
memValToString (VFun (_, _, args, t)) = "Function (" ++ argsToString args ++ ") -> " ++ show t

argsToString :: [Arg] -> String
argsToString [] = ""
argsToString [x] = argToString x
argsToString (x:xs) = argToString x ++ ", " ++ argsToString xs


identToString :: Ident -> String
identToString (Ident s) = s

argToString :: Arg -> String
argToString (VArg _ t i) = identToString i
argToString (PArg _ t i) = identToString i

updateEnv :: Env -> Arg -> MemVal -> RSEI Env
updateEnv env arg expr = do
    l <- newloc'
    case arg of
        VArg _ _ i -> do
            l' <- newloc'
            modify (M.insert l' expr)
            return (M.insert (argToString arg) l' env)
        PArg _ _ i -> do
            l' <- getLocation i
            return (M.insert (argToString arg) l' env)

prepareFuncEnv :: Env -> [Arg] -> [MemVal] -> RSEI Env
prepareFuncEnv env (arg : args) (expr : exprs) = do
    updatedEnv <- updateEnv env arg expr
    prepareFuncEnv updatedEnv args exprs
prepareFuncEnv env [] [] = do
    return env

runFunction :: Ident -> [Expr] -> BNFC'Position -> RSEI RetVal
runFunction ident args position = do
    case ident of
        Ident "print" -> printValue args
        _ -> do
            loc <- getLocation ident
            state <- get
            env <-ask
            case M.lookup loc state of
                Just (VFun (stmt, funcEnv, arg, resType)) -> do
                    funcArgs <- mapM eval args
                    modifyFunEnv <- prepareFuncEnv funcEnv arg funcArgs
                    (newEnv, value) <- local (const modifyFunEnv) (interpretStatemets stmt)
                    return value
                _ -> return Nothing


eval :: Expr -> RSEI MemVal
eval (ELitInt _ int) = do
    return (VInt int)

eval (ELitTrue _) = do
    return (VBool True)

eval (ELitFalse _) = do
    return (VBool False)

eval (EString _ string) = do
    return (VString string)

eval (Neg _ expr) = do
    VInt n <- eval expr
    return (VInt $ -n)

eval (Not _ expr) = do
    VBool e <- eval expr
    return (VBool $ not $ e)

eval (EAdd _ e1 op e2) = do
    VInt n1 <- eval e1
    VInt n2 <- eval e2
    case op of
        (Plus _ ) -> return $ VInt (n1 + n2)
        (Minus _) -> return $ VInt (n1 - n2)

eval (EMul _ e1 op e2) = do
    VInt n1 <- eval e1
    VInt n2 <- eval e2
    case op of
        (Div position)-> if n2 == 0 then throwError (DivByZero position) else return (VInt $ div n1 n2)
        (Mod position)-> if n2 == 0 then throwError (ModByZero position) else return (VInt $ n1 * n2)
        (Times _)-> return (VInt $ n1 * n2)

eval (ERel _ e1 op e2) = do
    VInt n1 <- eval e1
    VInt n2 <- eval e2
    case op of
        (LTH _) -> return $ VBool ( n1 < n2);
        (LE _) -> return $ VBool ( n1 <= n2);
        (GTH _) -> return $ VBool ( n1 > n2);
        (GE _) -> return $ VBool ( n1 >= n2);
        (EQU _) -> return $ VBool ( n1 == n2);
        (NE _) -> return $ VBool ( n1 /= n2);

eval (EAnd _ e1 e2) = do
    VBool n1 <- eval e1
    if not n1
        then return (VBool False)
        else eval e2

eval (EOr _ e1 e2) = do
    VBool n1 <- eval e1
    if n1
        then return (VBool True)
        else eval e2


eval (EVar _ name) = do
    loc <-  getLocation name
    state <- get
    let value = fromMaybe (VInt 0) (M.lookup loc state)
    return value

eval (ELam _ args ty (SBlock _ stmts)) = do
    env <- ask
    let funBody = (stmts, env, args, ty)
        funValue = VFun funBody
    return funValue

eval (EApp position ident args) =  do
    ret <- runFunction ident args position
    case ret of
        Just val -> return val
        Nothing -> return (VInt 0)

--Lokacja zawsze istnieje więc nigdy nie wyjdę do drugiego przypadku
getLocation :: Ident -> RSEI Int
getLocation (Ident x) = do
    env <- ask
    case M.lookup x env of
        Just l -> return l
        _ -> return 0

interpretBlock :: [Stmt] -> RSEI (Env,RetVal)
interpretBlock stmts = do
    env <- ask
    (_, ret) <- interpretStatemets stmts
    return (env, ret)

interpret :: Stmt -> RSEI (Env, RetVal)
interpret (Empty _) = do
    env <- ask
    return (env, Nothing)

interpret (BStmt _ (SBlock _ s )) = do
    env <- ask
    interpretBlock s
    return (env, Nothing)

interpret (Decl _ t []) = do
    env <- ask
    return (env, Nothing)

interpret (Decl position t ((Init _ (Ident x) s) : xs)) = do
    l <- newloc'
    w <- eval s
    modify (M.insert l w)
    env <- ask
    let updatedEnv = M.insert x l env
    local (const (M.insert x l env)) (interpret (Decl position t xs))

interpret (Ass _ s e) = do
    l <- getLocation s
    w <- eval e
    modify (M.insert l w)
    env <- ask
    return (env, Nothing)

interpret (VRet _) = do
    env <- ask
    return (env, Nothing)

interpret (Cond _ e (SBlock _ s)) = do
    env <- ask
    VBool x <- eval e
    if x then do
        ( _, ret) <- interpretStatemets s
        return (env, ret)
    else return (env, Nothing)

interpret (CondElse _ e (SBlock _ s1) (SBlock _ s2)) = do
    VBool x <- eval e
    env <- ask
    if x then do 
        (_, ret) <- interpretStatemets s1 
        return (env, ret)
    else do
        (_, ret) <- interpretStatemets s2 
        return (env, ret)


interpret (While position e s) = do
    VBool x <- eval e
    if x then do
        (newEnv, value) <- interpret s
        if isNothing value then
            interpret (While position e s)
        else
            return (newEnv, value)
    else do
        env <- ask
        return (env, Nothing)


interpret (SExp _ e) = do
    eval e
    env <- ask
    return (env, Nothing)

interpret (Ret _ e) = do
    env <- ask
    x <- eval e
    return (env, Just x)


interpret (FunExp _ (PFnDef _ t (Ident ident) args (SBlock _ stmts))) = do
    l <- newloc'
    env <- ask
    let updatedEnv = M.insert ident l env

    updatedEnv' <- funcEnv updatedEnv args

    let funBody = (stmts, updatedEnv', args, t)
        funValue = VFun funBody
    modify (M.insert l funValue)
    return (updatedEnv, Nothing)

funcEnv :: Env -> [Arg] -> RSEI Env
funcEnv = foldM (flip addSingleVar)

addSingleVar :: Arg -> Env -> RSEI Env
addSingleVar (PArg {} ) env = return env
addSingleVar (VArg _ typ (Ident x)) env = do
    l <- newloc'
    let updatedEnv = M.insert x l env
    return updatedEnv


interpretStatemets :: [Stmt] -> RSEI (Env, RetVal)
interpretStatemets [] = do
    env <- ask
    return (env, Nothing)

interpretStatemets (s:xs) = do
    (env, ret) <- interpret s
    case ret of
        Just _ -> return (env, ret)
        Nothing -> local (const env) (interpretStatemets xs)


exec :: Program -> IO (Either ExceptionsRunning Store)
exec (SProgram _ stmts) = runExceptT $ do
    execStateT (runReaderT (interpretStatemets stmts) env0) state0
        where
            state0 = M.empty
            env0 = M.empty
