
module RunProgram (exec) where

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import AbsGramatyka
import Data.Maybe



--wzorowałem się rozwiązaniem p.Chrząszcza z labów
type Loc  = Int

-- nie może być zmiennej o tej samej nazwie co funkcja
type Env  = M.Map String Loc

type Exceptions = Exceptions' BNFC'Position
data Exceptions' a = DivByZero a| ModByZero a | ReturnTypeError a deriving Show

type Store = M.Map Loc MemVal

data MemVal = VBool Bool | VInt Integer | VString String | VVoid | VFun FunBody deriving Show

data ArgTypeVal = Value TypeVal | Pointer TypeVal deriving (Eq, Show)

type RetVal = Maybe MemVal

data TypeVal = BoolV | IntV | StringV | VoidV | FunV [ArgTypeVal] TypeVal deriving (Eq, Show)

type FunBody = ([Stmt], Env, [Arg], Type)

type RSEI a = ReaderT Env (StateT Store (ExceptT Exceptions  IO)) a

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

castMemValToMyType :: MemVal -> TypeVal
castMemValToMyType (VBool _) = BoolV
castMemValToMyType (VInt _) = IntV
castMemValToMyType (VString _) = StringV
castMemValToMyType (VVoid) = VoidV
castMemValToMyType (VFun (_, _, args, t)) = FunV (map castArgToMyArgType args) (castToMyType t)

castRetValToMyType :: RetVal -> TypeVal
castRetValToMyType (Just value) = castMemValToMyType value
castRetValToMyType Nothing = VoidV

-- compareType :: Type -> Type -> Bool
-- compareType (Int _) (Int _) = True
-- compareType (Bool _) (Bool _) = True
-- compareType (Str _) (Str _) = True
-- compareType (Void _) (Void _) = True
-- compareType (Fun _ argTypes1 t1) (Fun _ argTypes2 t2) = compareType t1 t2 && compareArgTypes argTypes1 argTypes2

-- compareArgTypes :: [ArgType] -> [ArgType] -> Bool
-- compareArgTypes [] [] = True
-- compareArgTypes [arg1:args1] [arg2:args2] = compareArgType arg1 arg2 && compareArgTypes args1 args2
-- compareArgTypes _ _ = False

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

memValToString :: MemVal -> String
memValToString (VBool b) = if b then "True" else "False"
memValToString (VInt n) = show n
memValToString (VString s) = s

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
                    -- liftIO $ putStrLn $ "resType: " ++ show resType
                    let res = castToMyType resType
                    if res == castRetValToMyType value then return value else throwError (ReturnTypeError position)


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
    VBool n2 <- eval e2
    return (VBool $ n1 && n2)

eval (EOr _ e1 e2) = do
    VBool n1 <- eval e1
    VBool n2 <- eval e2
    return (VBool $ n1 || n2)


eval (EVar _ name) = do
    loc <-  getLocation name
    state <- get
    let value = fromMaybe (VInt 0) (M.lookup loc state)
    return value

eval (ELam _ args ty (SBlock _ stmts)) = do
    l <- newloc'
    env <- ask
    let funBody = (stmts, env, args, ty)
        funValue = VFun funBody
    modify (M.insert l funValue)
    return funValue

eval (EApp position ident args) =  do
    ret <- runFunction ident args position
    case ret of
        Just val -> return val
        Nothing -> return (VInt 0)

getLocation :: Ident -> RSEI Int
getLocation (Ident x) = do
    env <- ask
    case M.lookup x env of
        Just l -> return l

interpretBlock :: [Stmt] -> Env -> RSEI (Env,RetVal)
interpretBlock [] env = do
    return (env, Nothing)

interpretBlock (stmt:stmts) env = do
    (newEnv, retVal) <- interpret stmt
    local (const (M.union newEnv env)) $ do
        case retVal of
            Just _ -> return (newEnv, retVal)
            Nothing -> interpretBlock stmts newEnv

interpret :: Stmt -> RSEI (Env, RetVal)
interpret (BStmt _ (SBlock _ s )) = do
    env <- ask
    (finalEnv, _) <- interpretBlock s env
    return (finalEnv, Nothing)

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
    VBool x <- eval e
    env <- ask
    if x then interpretStatemets s else return (env, Nothing)

interpret (CondElse _ e (SBlock _ s1) (SBlock _ s2)) = do
    VBool x <- eval e
    env <- ask
    if x then interpretStatemets s1 else interpretStatemets s2

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


exec :: Program -> IO (Either Exceptions Store)
exec (SProgram _ stmts) = runExceptT $ do
    execStateT (runReaderT (interpretStatemets stmts) env0) state0
        where
            state0 = M.empty
            env0 = M.empty

-- z functorem zrobic


-- dalem blok zamiast Stmt w gramtyce
