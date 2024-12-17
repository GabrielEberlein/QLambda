module Main (main) where

import AST
import Eval

import System.IO (hFlush, stdout, readFile)
import Control.Monad (when)
import System.Random (newStdGen)
import Monad
import Parser
import Elab (elabProgram)

main :: IO ()
main = do
    putStrLn "Choose input method:"
    putStrLn "1. Enter input manually"
    putStrLn "2. Read input from file"
    putStrLn "3. Prueba"
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> manualInput
        "2" -> fileInput
        "3" -> prueba
        _   -> putStrLn "Invalid choice. Exiting."

manualInput :: IO ()
manualInput = do
    putStrLn "write term to parse"
    loop
    where
        loop :: IO ()
        loop = do
            putStr "> "
            hFlush stdout
            input <- getLine
            print $ parse input
            loop


fileInput :: IO ()
fileInput = do
    putStrLn "Enter the file path:"
    loop
    where
        loop :: IO ()
        loop = do
            hFlush stdout
            filePath <- getLine
            v<-runQState (loadFile $ "ejemplos/" ++ filePath) defEnv
            print v
            loop

loadFile :: MonadQuantum m => String -> m Value
loadFile f = do
     x <- liftIO $ readFile f
     sp <- case runP program x f of
            Left err -> throwError $ show err
            Right sp -> return sp
     p <- elabProgram sp
     evalProgram p

prueba :: IO ()
prueba = print "caca"