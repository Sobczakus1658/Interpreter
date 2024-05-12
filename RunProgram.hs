
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

data Exceptions = DivByZero | ModByZero | ReturnTypeError deriving Show

type Store = M.Map Loc MemVal 

data MemVal = BoolV Bool | IntV Integer | StringV String | VoidV | FunV FunBody deriving Show

type RetVal = Maybe MemVal

type FunBody = ([Stmt], Env, [Arg], Type)

type RSEI a = ReaderT Env (StateT Store (ExceptT Exceptions IO)) a 

evalAdd Plus e1 e2 = e1 + e2
evalAdd Minus e1 e2 = e1 - e2

evalMul Times e1 e2 = e1 * e2
evalMul Div e1 e2 = div e1 e2
evalMul Mod e1 e2 = mod e1 e2

evalRel LTH e1 e2 = e1 < e2 ;
evalRel LE e1 e2 = e1 <= e2;
evalRel GTH e1 e2 = e1 > e2;
evalRel GE e1 e2 = e1 >= e2;
evalRel EQU e1 e2 = e1 == e2 ;
evalRel NE e1 e2 = e1 /= e2;

newloc :: Store -> Loc 
newloc m =  if(M.null m) then 0 else let (i,w) = M.findMax m in i+1

newloc' :: RSEI Loc 
newloc' = do
    st <- get
    let l = newloc st
    return l

printValue :: [Expr] -> RSEI RetVal
printValue (x:xs) = do 
    w <- eval x
    liftIO $ putStrLn (memValToString w) 
    return (Nothing)

memValToString :: MemVal -> String
memValToString (BoolV b) = if b then "True" else "False"
memValToString (IntV n) = show n
memValToString (StringV s) = s

identToString :: Ident -> String
identToString (Ident s) = s

argToString :: Arg -> String
argToString (VArg t i) = identToString i
argToString (PArg t i) = identToString i

updateEnv :: Env -> Arg -> MemVal -> RSEI Env
updateEnv env arg expr = do
    l <- newloc' 
    case arg of
        VArg _ i -> do 
            l' <- newloc' 
            modify (M.insert l' expr)
            return (M.insert (argToString arg) l' env)  
        PArg _ i -> do
            l' <- getLocation i 
            return (M.insert (argToString arg) l' env)  

prepareFuncEnv :: Env -> [Arg] -> [MemVal] -> RSEI Env
prepareFuncEnv env (arg : args) (expr : exprs) = do
    -- liftIO $ putStrLn ("\nPrepareFunc: " ++ show arg ++ " " ++ show expr ++ " koniec \n")
    -- liftIO $ putStrLn ("\nFall Start PrepareFunc: " ++ show env ++ " koniec \n")
    updatedEnv <- updateEnv env arg expr
    -- liftIO $ putStrLn ("\nPrzed PrepareFunc: " ++ show updatedEnv ++ " koniec \n")
    updatedEnv' <- prepareFuncEnv updatedEnv args exprs  
    -- liftIO $ putStrLn ("\nPo PrepareFunc: " ++ show updatedEnv' ++ " koniec \n")
    return updatedEnv'
prepareFuncEnv env [] [] = do
    return env

runFunction :: Ident -> [Expr] -> RSEI RetVal
runFunction ident args = do
    case ident of 
        Ident "print" -> printValue args
        _ -> do
            -- liftIO $ putStrLn ("\nIdent : " ++ show ident ++ " koniec \n")
            loc <- getLocation ident
            -- liftIO $ putStrLn ("\nLokacja : " ++ show loc ++ " koniec \n")
            state <- get 
            env <-ask 
            case M.lookup loc state of
                Just (FunV (stmt, funcEnv, arg, resType)) -> do
                    -- liftIO $ putStrLn ("\nArg: " ++ show arg ++ " koniec \n")
                    funcArgs <- mapM eval args
                    -- liftIO $ putStrLn ("\nSiema: " ++ show state ++ " koniec \n")
                    modifyFunEnv <- prepareFuncEnv funcEnv arg funcArgs
                    -- liftIO $ putStrLn ("\nkotiwca : " ++ show modifyFunEnv ++ " koniec \n")
                    (newEnv, value) <- local (const modifyFunEnv) (interpretStatemets stmt)
                    -- liftIO $ putStrLn ("\nkotiwca : " ++ show resType ++ " koniec \n")
                    -- liftIO $ putStrLn ("\nEnvik : " ++ show env ++ " koniec \n")
                    if resType /= Void && isNothing value then throwError ReturnTypeError 
                    else  return value


eval :: Expr -> RSEI MemVal
eval (ELitInt int) = do 
    return (IntV int)

eval (ELitTrue) = do 
    return (BoolV True)

eval (ELitFalse) = do 
    return (BoolV False)

eval (EString string) = do 
    return (StringV string)

eval (Neg expr) = do 
    IntV n <- eval expr
    return (IntV $ -n)

eval (Not expr) = do 
    BoolV e <- eval expr
    return (BoolV $ not $ e)

eval (EAdd e1 op e2) = do 
    -- c <- eval e1 
    -- env <- ask
    -- state <- get
    -- liftIO  $ putStrLn ("\n\n Dodawanie \n\n" ++ show c ++  "\n\n " ++ show e2 ++ "\n")
    IntV n1 <- eval e1
    IntV n2 <- eval e2
    return (IntV $ evalAdd op n1 n2)

eval (EMul e1 op e2) = do 
    IntV n1 <- eval e1
    IntV n2 <- eval e2
    case op of
        Div -> if n2 == 0 then throwError DivByZero else return (IntV $ evalMul op n1 n2)
        Mod -> if n2 == 0 then throwError ModByZero else return (IntV $ evalMul op n1 n2)
        _ -> return (IntV $ evalMul op n1 n2)

eval (ERel e1 op e2) = do 
    IntV n1 <- eval e1
    IntV n2 <- eval e2
    return (BoolV $ evalRel op n1 n2)

eval (EAnd e1 e2) = do 
    BoolV n1 <- eval e1
    BoolV n2 <- eval e2
    return (BoolV $ n1 && n2)

eval (EOr e1 e2) = do 
    BoolV n1 <- eval e1
    BoolV n2 <- eval e2
    return (BoolV $ n1 || n2)


eval (EVar name) = do 
    loc <-  getLocation name
    state <- get
    let value = fromMaybe (IntV 0) (M.lookup loc state)
    return value

eval (ELam args ty (SBlock stmts)) = do 
    l <- newloc' 
    env <- ask
    let funBody = (stmts, env, args, ty)
        funValue = FunV funBody
    modify (M.insert l funValue)
    return funValue

eval (EApp ident args) =  do 
    ret <- runFunction ident args
    case ret of
        Just val -> return val
        Nothing -> return (IntV 0)

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

interpret (BStmt (SBlock s)) = do
    env <- ask
    (finalEnv, _) <- interpretBlock s env
    return (finalEnv, Nothing)

interpret (Decl t []) = do
    env <- ask
    return (env, Nothing)

interpret (Decl t ((Init (Ident x) s) : xs)) = do 
    l <- newloc' 
    w <- eval s 
    -- liftIO $ putStrLn ("Ident\n\n: " ++ x)
    -- liftIO $ putStrLn ("wartosc\n\n: " ++ show w)
    modify (M.insert l w)
    -- st <- get
    env <- ask
    let updatedEnv = M.insert x l env
    -- liftIO $ putStrLn ("nowyEnv: " ++ show st)
    local (const (M.insert x l env)) (interpret (Decl t xs))

interpret (Ass s e) = do 
    l <- getLocation s
    w <- eval e 
    modify (M.insert l w)
    env <- ask
    return (env, Nothing)

interpret (VRet) = do 
    env <- ask
    return (env, Nothing)

interpret (Cond e (SBlock s)) = do 
    BoolV x <- eval e 
    env <- ask
    if x then (interpretStatemets s) else return (env, Nothing)

interpret (CondElse e (SBlock s1) (SBlock s2)) = do 
    BoolV x <- eval e 
    env <- ask
    if x then (interpretStatemets s1) else (interpretStatemets s2)

interpret (While e s) = do 
    BoolV x <- eval e 
    if x then do
        (newEnv, value) <- interpret s
        if isNothing value then
            interpret (While e s)
        else
            return (newEnv, value)
    else do
        env <- ask
        return (env, Nothing)


interpret (SExp e) = do 
    eval e
    env <- ask
    return (env, Nothing)

interpret (Ret e) = do
    env <- ask
    x <- eval e
    return (env, Just x)


interpret (FunExp (PFnDef t (Ident ident) args (SBlock stmts))) = do
    l <- newloc' 
    env <- ask
    let updatedEnv = M.insert ident l env
        funBody = (stmts, updatedEnv, args, t)
        funValue = FunV funBody
    modify (M.insert l funValue)
    return (updatedEnv, Nothing)


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
exec (SProgram stmts) = runExceptT $ do
    execStateT (runReaderT (interpretStatemets stmts) env0) state0
        where
            state0 = M.empty 
            env0 = M.empty

-- jak pojawia się relacja to głupieje
-- naprawić example1

-- z functorem zrobic

-- dalem blok zamiast Stmt w gramtyce
