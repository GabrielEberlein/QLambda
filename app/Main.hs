module Main (main) where

import Parse -- Import the module generated by Happy
import AST
import Eval

import System.IO (hFlush, stdout, readFile)
import Control.Monad (when)
import System.Random (newStdGen)
import Monad 

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
    putStrLn "Enter input to parse (or type 'exit' to quit):"
    loop
  where
    loop :: IO ()
    loop = do
        putStr "> "         -- Prompt the user
        hFlush stdout       -- Ensure the prompt is displayed immediately
        input <- getLine    -- Read a line from standard input
        when (input /= "exit") $ do
            case stmts_parse input of
                Failed err -> print err
                Ok result -> print result
            loop

fileInput :: IO ()
fileInput = do
    putStrLn "Enter the file path:"
    hFlush stdout
    -- filePath <- getLine
    -- content <- readFile filePath
    content <- readFile "ejemplos/tp.ql"
    case stmts_parse content of
        Failed err -> print err
        Ok result -> do v <- runQState (evalStmt result) defEnv
                        print result >> print v

prueba :: IO ()
prueba = print "caca"

-- prueba :: IO ()
-- prueba = do v <- runQState eval defEnv
--             case v of
--                 Right (r, e) -> case r of
--                     VPair VOne VOne -> print (1,1) >> print e
--                     VPair VZero VZero -> print (0,0) >> print e
--                     _ -> print "No EPR" >> print e
--                 Left e -> print e

-- eval :: QState Value
-- eval = do i <- new 0
--           j <- new 0
--           apply H i
--           apply2 CNot i j
--           v2 <- meas j
--           v1 <- meas i
--           return $ VPair v1 v2