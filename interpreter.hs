module Interpreter (interpretFromFile, interpretFromInput, main) where

import           LexGramatyka    (Token)
import           ParGramatyka    (myLexer, pProgram)
import           AbsGramatyka
import           TypeChecker     (checkProgram)
import           RunProgram      (exec)
import           System.Environment (getArgs) 

data Exceptions = DivByZero | ModByZero | ReturnTypeError deriving Show


interpretFromFile :: FilePath -> IO ()
interpretFromFile file = readFile file >>= interpretFromInput

interpretFromInput :: String -> IO ()
interpretFromInput input = do
    let lexerTokens = myLexer input
        parsedTokens = pProgram lexerTokens
    case parsedTokens of
        Right program -> checkAndRunProgram program
        Left err -> putStrLn $ "Error during parsing: " ++ show err

checkAndRunProgram :: Program -> IO ()
checkAndRunProgram program = do
    result <- checkProgram program
    case result of
        Left err -> putStrLn $ "Type Error: " ++ show err
        Right _ -> do
            --   putStrLn $ show program
              outcome <- exec program
              case outcome of
                  Left err -> putStrLn $ "Error during running: " ++ show err
                  Right _ -> putStrLn $ "Exit"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> interpretFromFile file
        _      -> putStrLn "Usage: interpreter <filename>"
