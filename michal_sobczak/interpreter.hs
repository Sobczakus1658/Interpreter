module Interpreter (interpretFromFile, interpretFromInput, main) where

import           LexGramatyka    (Token)
import           ParGramatyka    (myLexer, pProgram)
import           AbsGramatyka
import           TypeChecker     (checkProgram)
import           RunProgram      (exec)
import           System.Environment (getArgs)

interpretFromFile :: FilePath -> IO ()
interpretFromFile file = readFile file >>= interpretFromInput

interpretFromInput :: String -> IO ()
interpretFromInput input = do
    let lexerTokens = myLexer input
        parsedTokens = pProgram lexerTokens
    case parsedTokens of
        Right program -> checkAndRunProgram program
        Left err -> print err

checkAndRunProgram :: Program -> IO ()
checkAndRunProgram program = do
    result <- checkProgram program
    case result of
        Left err -> print err
        Right _ -> do
              outcome <- exec program
              case outcome of
                  Left err -> print err
                  _ -> return ()
                  
main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> interpretFromFile file
        [] ->  getContents >>= interpretFromInput
        _ -> putStrLn "Usage: ./interpreter [file]"