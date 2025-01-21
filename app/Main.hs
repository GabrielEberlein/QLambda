module Main (main) where

import AST
import Eval

import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Monad
import Parser
import Elab (elabProgram, elab)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> interactive
        ("--help":_) -> printHelp
        ("--interactive":_) -> interactive
        ("--file":file:_) -> do _ <- runFile defState file
                                return ()
        _ -> do putStrLn $ "Invalid arguments "++ unwords args
                printHelp

printHelp :: IO ()
printHelp = do
    putStrLn "Usage: make run [--help | --interactive | --file <file>]"
    putStrLn "Options:"
    putStrLn "  --help          Show this help"
    putStrLn "  --interactive   Start the interactive mode"
    putStrLn "  --file <file>   Evaluate the file"

interactive :: IO ()
interactive = do
    putStr "Entered interactive mode.\n Write :h for all comands.\n"
    loop defState
    where
        loop :: PState -> IO ()
        loop st = do
            liftIO $ putStr "> "
            liftIO $ hFlush stdout
            input <- liftIO $ words <$> getLine
            case input of
                [] -> loop st
                (":q":_) -> return ()
                (":h":_) -> do
                    putStrLn "Commands:"
                    putStrLn ":q - Quit"
                    putStrLn ":h - Show this help"
                    putStrLn ":l <file> - Load a file"
                    putStrLn ":p <decl/term> - Parse a term or declaration"
                    putStrLn "<decl/term> - Evaluate a term or declaration"
                    loop st
                (":l":file:_) -> runFile st file >>= loop
                (":p":xs) -> do let p = parse $ unwords xs
                                print p
                                loop st
                xs -> do st' <- runTerm st $ unwords xs
                         loop st'

runFile :: PState -> String -> IO PState
runFile st file = do
    result <- runQState (evalFile file) st
    case result of
        Left err -> putStrLn err >> return st
        Right (a,st') -> putStrLn "" >>print a >> return st'

runTerm :: PState -> String -> IO PState
runTerm st term = do
    result <- runQState (evalTerm term) st
    case result of
        Left err -> putStrLn err >> return st
        Right (a,st') -> print a >> return st'

evalFile :: MonadQuantum m => String -> m Value
evalFile f = do
     x <- liftIO $ readFile f
     sp <- case runP program x f of
            Left err -> throwError $ show err
            Right sp -> return sp
     p <- elabProgram sp
     evalProgram p

evalTerm :: MonadQuantum m => String -> m Value
evalTerm t = do
    case runP declOrTm t "stdin" of
        Left err -> throwError $ show err
        Right a -> case a of
            Left d -> elabProgram [d] >>= evalProgram
            Right t' -> elab [] t' >>= eval