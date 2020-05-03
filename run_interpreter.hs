module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Trans.Except

import LexGrammar
import ParGrammar
import Interpreter
import PrintGrammar
import AbsGrammar
import StaticCheck

import ErrM

type LexerFun = String -> [Token]
type ParserFun = [Token] -> Err (Program Position)
type SCheckerFun = Program Position -> SCResult ()

type Verbosity = Int

lexer = myLexer
parser = pProgram
checker = sCheck
interpreter = interpretProgram

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> LexerFun -> ParserFun -> SCheckerFun -> FilePath -> IO ()
runFile v l p sc f = putStrLn f >> readFile f >>= run v l p sc

showTree :: Int -> Program Position -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

printParseError :: Verbosity -> [Token] -> String -> IO ()
printParseError v tokens err = do putStrLn "\nParse failed...\n"
                                  putStrV v "Tokens:"
                                  putStrV v $ show tokens
                                  putStrLn err

printParseSuccess :: Verbosity -> Program Position -> IO ()
printParseSuccess v tree = do putStrLn "\nParsing successful!\n"
                              showTree v tree

printStaticCheckError :: SCError -> IO ()
printStaticCheckError err = do putStrLn "\nStatic checking failed...\n"
                               putStrLn $ show err

printStaticCheckSuccess :: IO ()
printStaticCheckSuccess = putStrLn "Static checking successful!"

printInterpreterError :: RuntimeError -> IO ()
printInterpreterError err = do putStrLn "\nRuntime error occurred...\n"
                               putStrLn $ show err

run :: Verbosity -> LexerFun -> ParserFun -> SCheckerFun -> String -> IO ()
run v l p sc s = let tokens = l s in case p tokens of
  Bad err -> printParseError v tokens err >> exitFailure
  Ok tree -> do printParseSuccess v tree
                scResult <- runExceptT $ sc tree
                case scResult of
                  Left err -> printStaticCheckError err >> exitFailure
                  Right _ -> printStaticCheckSuccess
                interpreterResult <- interpretProgram tree
                case interpreterResult of
                  Left err -> do printInterpreterError err >> exitFailure
                  Right _ -> printInterpreterSuccess >> exitSuccess

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Interpret stdin verbosely."
    , "  (files)         Interpret content of files verbosely."
    , "  -s (files)      Silent mode. Interpret content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 lexer parser checker
    "-s":fs -> mapM_ (runFile 0 lexer parser checker) fs
    fs -> mapM_ (runFile 2 lexer parser checker) fs