
module RunProgram (exec) where
    
import qualified Data.Map as M 
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Except
import AbsGramatyka
import Data.Maybe
-- import Data.Maybe(fromMaybe, Maybe(Nothing))


--wzorowałem się rozwiązaniem p.Chrząszcza z labów
type Loc  = Int

type Env  = M.Map String Loc 

-- type PEnv = M.Map String Loc 

-- data EnvPair = EnvPair { env :: Env, penv :: PEnv }


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
-- --do usuniecia
-- instance Show MemVal where
--     show (BoolV b) = "BoolVal " ++ show b
--     show (IntV i) = "IntVal " ++ show i
--     show (StringV s) = "StringVal " ++ show s
--     -- show (StringVal s) = "StringVal " ++ show s
--     show VoidV = "VoidVal"  -- Brakuje obsługi VoidVal
--     show (FunV funBody) = "Function" ++ show funBody

newloc :: Store -> Loc 
newloc m =  if(M.null m) then 0 else let (i,w) = M.findMax m in i+1

newloc' :: RSEI Loc 
newloc' = do
    st <- get
    let l = newloc st
    return l

printValue :: [Expr] -> RSEI RetVal
printValue (x:xs) = do 
    state <- ask 
    -- liftIO $ putStrLn ("pizda \n" ++ show x ++ "\n\n")
    w <- eval x
    liftIO $ putStrLn (show w)
    return (Just (IntV 42))

identToString :: Ident -> String
identToString (Ident s) = s

argToString :: Arg -> String
-- argToString a = "chuj"
argToString (VArg t i) = identToString i
argToString (PArg t i) = identToString i

-- tutaj trzeba zaalkować nową pamięc i przypisać jej wartosc
-- prepareFuncEnv :: Env -> [Arg] -> [MemVal] -> RSEI Env
-- prepareFuncEnv env args exprs = do
--     liftIO $ putStrLn ("argumenty:\n\n " ++ show args)
--     liftIO $ putStrLn ("wartosci: \n\n" ++ show exprs)
--     return env


-- cleanIdentifier :: String -> String
-- cleanIdentifier str = case reads str :: [(String, String)] of
--     [(clean, "")] -> clean
--     _ -> str  -- W przypadku błędu, zwróć oryginalny ciąg znaków

-- prepareFuncEnv env ( arg :args) (expr:exprs) = do

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
    liftIO $ putStrLn ("\nFall Start PrepareFunc: " ++ show env ++ " koniec \n")
    updatedEnv <- updateEnv env arg expr
    liftIO $ putStrLn ("\nPrzed PrepareFunc: " ++ show updatedEnv ++ " koniec \n")
    updatedEnv' <- prepareFuncEnv updatedEnv args exprs  
    liftIO $ putStrLn ("\nPo PrepareFunc: " ++ show updatedEnv' ++ " koniec \n")
    return updatedEnv'

        -- musze odczytać na co wskazuje 
    -- liftIO $ putStrLn ("zmienna :\n\n " ++ show (argToString arg))
    -- let updatedEnv = M.insert ( argToString arg) l env 
    -- updatedEnv' <- prepareFuncEnv updatedEnv args exprs  
    -- return updatedEnv'
    -- return env

prepareFuncEnv env [] [] = do
    -- state <- get
    -- liftIO $ putStrLn ("env:\n\n " ++ show env)
    -- liftIO $ putStrLn ("state: \n\n" ++ show state)
    return env
-- prepareFuncEnv env _ _ = throwError "Błąd: Niezgodna liczba argumentów i wartości"

-- funv Env ma inne środowisko niż env
runFunction :: Ident -> [Expr] -> RSEI RetVal
runFunction ident args = do
    case ident of 
        Ident "print" -> printValue args
        _ -> do
            liftIO $ putStrLn ("\nIdent : " ++ show ident ++ " koniec \n")
            loc <- getLocation ident
            -- liftIO $ putStrLn ("\nLokacja : " ++ show loc ++ " koniec \n")
            state <- get 
            env <-ask 
            case M.lookup loc state of
                Just (FunV (stmt, funcEnv, arg, resType)) -> do
                    -- liftIO $ putStrLn ("\nArg: " ++ show arg ++ " koniec \n")
                    funcArgs <- mapM eval args
                    liftIO $ putStrLn ("\nSiema: " ++ show state ++ " koniec \n")
                    modifyFunEnv <- prepareFuncEnv funcEnv arg funcArgs
                    liftIO $ putStrLn ("\nkotiwca : " ++ show modifyFunEnv ++ " koniec \n")
                    (newEnv, value) <- local (const modifyFunEnv) (interpretStatemets stmt)
                    -- liftIO $ putStrLn ("\nkotiwca : " ++ show resType ++ " koniec \n")
                    liftIO $ putStrLn ("\nEnvik : " ++ show env ++ " koniec \n")
                    if resType /= Void && isNothing value then throwError ReturnTypeError 
                    else  return value


eval :: Expr -> RSEI MemVal
eval (ELitInt int) = do 
    -- liftIO $ putStrLn ("Wartosc x: " ++ show int)
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
    c <- eval e1 
    env <- ask
    state <- get
    liftIO  $ putStrLn ("\n\n Dodawanie \n\n" ++ show c ++  "\n\n " ++ show e2 ++ "\n")
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
    -- liftIO $ putStrLn ("Lokacja x: " ++ show loc)
    state <- get
    -- liftIO $ putStrLn ("Lokacja x: " ++ show state)
    let value = fromMaybe (IntV 0) (M.lookup loc state)
    -- liftIO $ putStrLn ("Lokacjaaaaaaa x: " ++ show value)
    return value

-- eval (EAdd e1 Plus e2) = do 
--     IntV val1 <- eval e1 
--     IntV val2 <- eval e2 
--     -- liftIO $ putStrLn ("Dodawanie: ")
--     return $ IntV (val1 + val2)

eval (ELam args ty (SBlock stmts)) = do 
    l <- newloc' 
    env <- ask
    let funBody = (stmts, env, args, ty)
        funValue = FunV funBody
    modify (M.insert l funValue)
    return funValue


    -- IntV val1 <- eval e1 
    -- IntV val2 <- eval e2 
    -- liftIO $ putStrLn ("lambda: \n\n\n") 
    -- liftIO $ putStrLn show block 
    -- liftIO $ putStrLn ("Block: " ++ show block)
    -- env <- ask
    -- newEnv <- foldM updateEnv env args
    -- return (IntV 42)


eval (EApp ident args) =  do 
    -- liftIO  $ putStrLn ("\n\n chujas \n\n" ++ show ident ++ " " ++ show args)
    -- value <- runFunction ident args
    -- return value
    -- funcArgs <- mapM eval args
    ret <- runFunction ident args

    case ret of
        Just val -> return val
        Nothing -> return (IntV 42)

-- eval x =  do 
--     -- liftIO  $ putStrLn ("\n chuj \n" ++ show x)
--     return (IntV 42)

getLocation :: Ident -> RSEI Int
getLocation (Ident x) = do
    -- liftIO $ putStrLn ("Location x: " ++ show x)
    env <- ask
    -- liftIO $ putStrLn ("Location env : " ++ show env)
    -- env <- ask
    case M.lookup x env of
        Just l -> return l
        -- Nothing -> throwError $ "Undefined variable: " ++ x


-- updateEnv :: Env -> Arg -> RSEI Env
-- updateEnv env (VArg argName argValue) = do
--     return env

initValues :: [Arg] -> RSEI Env
initValues args = do 
    env <- ask
    return env
    -- updatedEnv <- foldM updateEnv env args
    -- return updatedEnv

interpretBlock :: [Stmt] -> Env -> RSEI (Env, RetVal)
interpretBlock [] env = do
    -- liftIO $ putStrLn ("srodowisko blok: " ++ show env)
    return (env, Nothing)

interpretBlock (stmt:stmts) env = do
    -- liftIO $ putStrLn ("\n\n srodowisko  i blok: " ++ show stmt ++ " \n" ++ show env)
    -- modify(const env)
    (newEnv, retVal) <- interpret stmt
    -- liftIO $ putStrLn ("\n\n po interpreterze" ++  show newEnv)
    -- modify (const newEnv)
    local (const (M.union newEnv env)) $ do
        case retVal of
            Just _ -> return (newEnv, retVal)
            Nothing -> interpretBlock stmts newEnv

-- interpret (Print e) = do
--     liftIO $ putStrLn ("chuj: ")
--     env <- ask
--     -- liftIO $ putStrLn ("srodowisko ostatni: " ++ show env)
--     return (env, Nothing)
    -- res <- evalExpression e
    -- let str = memValToString res
    -- liftIO $ putStr str
    -- returnNothing

interpret (BStmt (SBlock s)) = do
    -- liftIO $ putStrLn ("chujas\n\n: ")
    env <- ask
    (finalEnv, _) <- interpretBlock s env
    -- liftIO $ putStrLn ("chuj: " ++ show finalEnv)
    return (finalEnv, Nothing)

-- interpretBlock :: [Stmt] -> Env -> RSEI (Env, RetVal)
-- interpretBlock [] env = return (env, Nothing)
-- interpretBlock (stmt:stmts) env = do
--     (newEnv, retVal) <- interpret stmt
--     case retVal of
--         Just _ -> return (newEnv, retVal)
--         Nothing -> interpretBlock stmts newEnv

-- -- interpret :: Stmt -> RSEI (Env, RetVal) 
-- interpret (BStmt (SBlock s)) = do
--     -- liftIO $ putStrLn "chuj \n\n"
--     mapM_ interpret s 
--     env <- ask
--     return (env, Nothing)

interpret (Decl t []) = do
    -- liftIO $ putStrLn ("koniec juz\n\n")
    env <- ask
    -- liftIO $ putStrLn ("srodowisko ostatni: " ++ show env)
    return (env, Nothing)

interpret (Decl t ((Init (Ident x) s) : xs)) = do 
    l <- newloc' 
    w <- eval s 
    liftIO $ putStrLn ("Ident\n\n: " ++ x)
    liftIO $ putStrLn ("wartosc\n\n: " ++ show w)
    modify (M.insert l w)
    st <- get
    env <- ask
    let updatedEnv = M.insert x l env
    liftIO $ putStrLn ("nowyEnv: " ++ show st)
    local (const (M.insert x l env)) (interpret (Decl t xs))

interpret (Ass s e) = do 
    env <- ask
    st <- get
    -- liftIO $ putStrLn ("Assign Sro" ++ show env)
    -- liftIO $ putStrLn ("Assign Sta" ++ show st)
    l <- getLocation s
    w <- eval e 
    modify (M.insert l w)
    env <- ask
    return (env, Nothing)

interpret (VRet) = do 
    env <- ask
    return (env, Nothing)

interpret (Cond e s) = do 
    BoolV x <- eval e 
    env <- ask
    if x then (interpret s) else return (env, Nothing)

interpret (CondElse e s1 s2) = do 
    BoolV x <- eval e 
    env <- ask
    if x then (interpret s1) else (interpret s2)

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
    -- liftIO $ putStrLn ("Bloczek \n\n" ++ show block ++ "\n Koniec \n")
    let updatedEnv = M.insert ident l env
        -- stmts = []
        -- arg = []
        funBody = (stmts, updatedEnv, args, t)
        funValue = FunV funBody

    -- let updatedEnv = M.insert ident l env
    -- let args = []
    -- let stmts = []
    -- liftIO $ putStrLn (show block ++ "\n Koniec \n")
    -- let funValue = FunV (stmts, env, args, t, [])
    modify (M.insert l funValue) -- Dodaj funkcję do pamięci
    -- let newEnv = ... -- Utwórz nowe środowisko dla funkcji, uwzględniając argumenty
    -- local (const env) (interpret (BStmt block)) 
    --         local (const env) (putToMemory ident (FunVal (stmts, env, argsList, returnType, [])))
    return (updatedEnv, Nothing)
    -- ogónie to trzeba jakoś enva zmienić żeby powiedzieć co trzeba zrobić jak się wywoła fuunkcje 
    -- newEnv <- initValues argsl (const env) 
    -- let updatedEnv = M.insert ident l env
    -- let value = FunVal(FunVal (stmts, env, argsList, returnType, [])
    -- modify (M.insert l value)
    -- env <- declareVar ident
    -- liftIO $ putStrLn ("\nBlok\n" ++ show block)
    -- liftIO $ putStrLn ("\n srodowisko \n" ++ show newEnv)
    -- liftIO $ putStrLn "\n Koniec \n"
    -- local (const newEnv) (interpret (BStmt block))


interpretStatemets :: [Stmt] -> RSEI (Env, RetVal)
interpretStatemets [] = do 
    env <- ask
    return (env, Nothing)
     -- Jeśli lista jest pusta, zwróć Nothing
interpretStatemets (s:xs) = do
    (env, ret) <- interpret s -- Wykonaj pierwszą instrukcję
    case ret of
        Just _ -> return (env, ret) -- Jeśli otrzymano wartość zwrotną, zwróć ją
        Nothing -> local (const env) (interpretStatemets xs) -- W przeciwnym razie wykonaj resztę instrukcji

exec :: Program -> IO (Either Exceptions Store) 
exec (SProgram stmts) = runExceptT $ do
    execStateT (runReaderT (interpretStatemets stmts) env0) state0
        where
            state0 = M.empty 
            env0 = M.empty


--typechecker ( niepoprawna liczba argumentów, niezadeklarowana zmienna)

-- wykrywać maina i go wykonywać