{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Main (main) where

import AST
import Eval

import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Monad
import Parser
import PrettyPrinter (pp)
import Elab (elabProgram, elab)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-h":_)            -> printHelp
        ("--help":_)        -> printHelp

        ("-i":_)            -> interactive
        ("--interactive":_) -> interactive

        ("-f":file:_)       -> runFile defState file >> return ()
        ("--file":file:_)   -> runFile defState file >> return ()

        _ -> do putStrLn $ "Invalid arguments "++ unwords args
                printHelp

printHelp :: IO ()
printHelp = do
    putStrLn "Usage: stack run -- [OPTIONS]"
    putStrLn "[OPTIONS]:"
    putStrLn "  (--help/-h)          Show this help"
    putStrLn "  (--file/-f) <file>   Evaluate the file"
    putStrLn "  (--interactive/-i)   Start the interactive mode"

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
                    putStrLn ":q             - Quit"
                    putStrLn ":h             - Show this help"
                    putStrLn ":st            - Show the current quantum state"
                    putStrLn ":c             - Clear the quantum state and defined variables"
                    putStrLn ":l <file>      - Load a file"
                    putStrLn ":p <decl/term> - Parse a term or declaration"
                    putStrLn "<decl/term>    - Evaluate a term or declaration"
                    loop st
                (":st":_) -> printState st >>= loop
                (":c":_) -> putStrLn "State cleared" >> loop defState
                (":l":file:_) -> runFile st file >>= loop
                (":p":xs) -> do let p = runP declOrTm (unwords xs) "stdin"
                                case p of
                                    Left err -> print err
                                    Right p' -> case p' of
                                        Left d -> print d
                                        Right t -> print t
                                loop st
                xs -> do st' <- runTerm st $ unwords xs
                         loop st'

printState :: PState -> IO PState
printState st = do 
    result <- runQState ppState st
    case result of
        Left err -> putStrLn err >> return st
        Right (_,st') -> return st'

runFile :: PState -> String -> IO PState
runFile st file = do
    result <- runQState (evalFile file) st
    case result of
        Left err -> putStrLn err >> return st
        Right (a,st') -> putStrLn "" >>pp a >> return st'

runTerm :: PState -> String -> IO PState
runTerm st term = do
    result <- runQState (evalTerm term) st
    case result of
        Left err -> putStrLn err >> return st
        Right (a,st') -> pp a >> return st'

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