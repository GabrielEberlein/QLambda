module PrettyPrinter (
    prettyQbits,
    pp
) where

import Qbit
import AST
import Numeric (showBin)

prettyBits :: Int -> Int -> String
prettyBits i n = let bitwise = showBin i ""
                     zeros = replicate (n - length bitwise) '0'
                  in zeros ++ bitwise

prettyQbits :: Int -> Int -> [Complex Double] -> String
prettyQbits _ 0 _ = "No qbits"
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

ppTerm :: Term -> String
ppTerm (C c) = show c
ppTerm Ople = "*"
ppTerm (Print s t) = "print " ++ s ++ " " ++ ppTerm t
ppTerm (QBit i) = "|q" ++ show i ++ "⟩"
ppTerm (Bound i) = "b" ++ show i
ppTerm (Free n) = n
ppTerm (Abs t) = "λ." ++ ppTerm t
ppTerm (App t1 t2) = "(" ++ ppTerm t1 ++ " " ++ ppTerm t2 ++ ")"
ppTerm (Pair t1 t2) = "⟨" ++ ppTerm t1 ++ ", " ++ ppTerm t2 ++ "⟩"
ppTerm (Let m n) = "let " ++ ppTerm m ++ " in " ++ ppTerm n
ppTerm (Rec m n) = "rec " ++ ppTerm m ++ " in " ++ ppTerm n
ppTerm Zero = "0"
ppTerm One = "1"
ppTerm (InjL t) = "injL " ++ ppTerm t
ppTerm (InjR t) = "injR " ++ ppTerm t
ppTerm (Match t l r) = "match " ++ ppTerm t ++ " with injL " ++ ppTerm l ++ " injR " ++ ppTerm r

pp :: Value -> IO ()
pp v = putStrLn $ ppTerm $ quote v
      where quote (VC c) = C c
            quote VOple = Ople
            quote (VAbs t) = Abs t
            quote (VInjL v') = InjL (quote v')
            quote (VInjR v') = InjR (quote v')
            quote (VPair v1 v2) = Pair (quote v1) (quote v2)
            quote (VQbit i) = QBit i

