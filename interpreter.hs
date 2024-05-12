module Interpreter (interpretFromFile, interpretFromInput) where

import           LexGramatyka    (Token)
import           ParGramatyka    (myLexer, pProgram)
import           AbsGramatyka
import           TypeChecker     (checkProgram)
import           RunProgram      (exec)

data Exceptions = DivByZero | ModByZero | ReturnTypeError deriving Show


interpretFromFile :: FilePath -> IO ()
interpretFromFile file = readFile file >>= interpretFromInput

interpretFromInput :: String -> IO ()
interpretFromInput input = do
    let lexerTokens = myLexer input
        parsedTokens = pProgram lexerTokens
    case parsedTokens of
        Right program -> checkAndRunProgram program
        Left err -> putStrLn $ "Błąd typowania programu: " ++ show err

checkAndRunProgram :: Program -> IO ()
checkAndRunProgram program = do
    -- Left err -> putStrLn $ "Błąd typowania: " ++ err
    -- Right program -> do
      result <- checkProgram program
      case result of
          Left err -> putStrLn $ "Błąd typowania programu: " ++ show err
          Right _ -> do
              putStrLn $ show program
              outcome <- exec program
              -- putStrLn $ "Koniec"
              -- putStrLn $ show result
              case outcome of
                  Left err -> putStrLn $ "Błąd wykonywania programu: " ++ show err
                  Right _ -> putStrLn $ "Koniec"
                  -- Right x ->  putStrLn $ show x

