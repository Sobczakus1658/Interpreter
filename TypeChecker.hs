module TypeChecker (checkProgram) where

import qualified Data.Map as M 
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Except
import AbsGramatyka
import Data.Maybe

type Loc  = Int

type Env  = M.Map String Loc 

data Exceptions = DivByZero | ModByZero | ReturnTypeError deriving Show

-- type Store = M.Map Loc MemVal 

data TypeT = BoolT Bool | IntT Integer | StringT String | VoidT | FunT FunBody deriving Show

type RetVal = Maybe TypeT

type FunBody = ([Stmt], Env, [Arg], TypeT)

type REI = ReaderT Env (ExceptT Exceptions IO) 

isCorrectType :: TypeT -> TypeT -> TypeT
isCorrectType a b = BoolT True

check :: Stmt -> REI (Env, RetVal)
check _ = do     
    env <- ask
    return (env, Nothing)

checkStatemets :: [Stmt] -> REI (Env, RetVal)
checkStatemets [] = do
    env <- ask
    return (env, Nothing)
checkStatemets (s:xs) = do
    (env, ret) <- check s
    case ret of
        Just _ -> return (env, ret)
        Nothing -> local (const env) (checkStatemets xs)

checkProgram :: Program -> IO (Either Exceptions ()) 
checkProgram (SProgram stmts) = runExceptT $ do
    void $ runReaderT (checkStatemets stmts) env0
  where
    env0 = M.empty 