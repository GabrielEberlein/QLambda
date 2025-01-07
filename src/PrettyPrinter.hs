module PrettyPrinter (
    prettyQbits
) where

import Qbit
import Numeric (showBin)

prettyBits :: Int -> Int -> String
prettyBits i n = let bitwise = showBin i "" 
                     zeros = replicate (n - length bitwise) '0'
                  in zeros ++ bitwise

prettyQbits :: Int -> Int -> [Complex Double] -> String
prettyQbits _ _ [] = ""
prettyQbits i n (x:xs) = let a  = realPart x
                             b  = imagPart x
                             i' = prettyBits i n
                             ss = prettyQbits (i+1) n xs
                             s | a+b == 0 = ""
                               | a+b == a = show a ++ " |" ++ i' ++ "⟩"
                               | a+b == b = show b ++ "i |" ++ i' ++ "⟩"
                               | otherwise = "(" ++ show a ++ " + " ++ show b ++ "i) |" ++ i' ++ "⟩"
                         in if ss == "" then s else
                              if s == "" then ss else 
                                s ++ " + " ++ ss
