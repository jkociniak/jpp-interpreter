module Main where

import System.IO (stdin, stderr, hPutStrLn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.Except
import Control.Monad.Trans.Except

import LexGrammar
import ParGrammar
import Interpreter
import PrintGrammar
import AbsGrammar
import StaticCheck
import Interpreter

import ErrM

type LexerFun = String -> [Token]
type ParserFun = [Token] -> Err (Program Position)
type SCheckerFun = Program Position -> SCResult ()
type InterpreterFun = Program Position -> IResult Value
type Code = String

data Settings = Settings LexerFun ParserFun SCheckerFun InterpreterFun
defaultSettings :: Settings
defaultSettings = Settings myLexer pProgram sCheck interpretProgram

runFile :: Settings -> FilePath -> IO ()
runFile s f = putStrLn f >> readFile f >>= run s

showTree :: Program Position -> IO ()
showTree tree = do
    putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

printParseError :: [Token] -> String -> IO ()
printParseError tokens err = do 
    hPutStrLn stderr "\nParse failed...\n"
    hPutStrLn stderr "Tokens:"
    hPutStrLn stderr $ show tokens
    hPutStrLn stderr err

printParseSuccess :: Program Position -> IO ()
printParseSuccess tree = putStrLn "\nParsing successful!\n" >> showTree tree

printStaticCheckError :: SCError -> IO ()
printStaticCheckError err = hPutStrLn stderr "\nStatic checking failed...\n" >> hPutStrLn stderr (show err)

printStaticCheckSuccess :: IO ()
printStaticCheckSuccess = putStrLn "Static checking successful!"

printInterpreterError :: RuntimeError -> IO ()
printInterpreterError err = 
    hPutStrLn stderr "\nRuntime error occurred...\n" >> hPutStrLn stderr (show err)

printInterpreterSuccess :: Value -> IO ()
printInterpreterSuccess ret = putStrLn $ "Program finished with return value " ++ show ret

run :: Settings -> Code -> IO ()
run (Settings lexer parser checker interpreter) code = 
    let tokens = lexer code in case parser tokens of
        Bad err -> printParseError tokens err >> exitFailure
        Ok tree -> do
            printParseSuccess tree
            scResult <- runExceptT $ checker tree
            case scResult of
                Left err -> printStaticCheckError err >> exitFailure
                Right _ -> printStaticCheckSuccess
            interpreterResult <- runExceptT $ interpreter tree
            case interpreterResult of
                Left err -> printInterpreterError err >> exitFailure
                Right ret -> printInterpreterSuccess ret >> exitSuccess

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: Call with one of the following argument combinations:"
        , "  --help          Display this help message."
        , "  (no arguments)  Interpret stdin verbosely."
        , "  (files)         Interpret content of files verbosely."
        , "  -s (files)      Silent mode. Interpret content of files silently."
        ]
    exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        [] -> getContents >>= run defaultSettings
        "-s":fs -> mapM_ (runFile defaultSettings) fs
        fs -> mapM_ (runFile defaultSettings) fs