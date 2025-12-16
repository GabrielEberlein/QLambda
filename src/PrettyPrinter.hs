module PrettyPrinter (
    prettyState,
    pp
) where

import Qbit
import AST
import Numeric (showBin)

prettyBits :: Int -> Int -> String
prettyBits i n = let bitwise = showBin i ""
                     zeros = replicate (n - length bitwise) '0'
                  in zeros ++ bitwise


-- Funcion para formatear el estado de los cubits
prettyStateDirac :: Int -> Int -> [Complex Double] -> String
prettyStateDirac _ 0 _  = "No qbits"
prettyStateDirac _ _ [] = ""
prettyStateDirac i n (x:xs) =
  let a  = realPart x
      b  = imagPart x
      i' = prettyBits i n
      ss = prettyStateDirac (i+1) n xs
      s
        | a + b == 0 = ""
        | a + b == a = show a ++ " |" ++ i' ++ "⟩"
        | a + b == b = show b ++ "i |" ++ i' ++ "⟩"
        | otherwise  = "(" ++ show a ++ " + " ++ show b ++ "i) |" ++ i' ++ "⟩"
  in
    if ss == "" then s
    else if s == "" then ss
    else s ++ " + " ++ ss


bar :: Double -> Int -> String
bar p ancho = let filled = round (p * fromIntegral ancho) in replicate filled '█' ++ replicate (ancho - filled) '░'

prettyStateAmplitudes :: Int -> Int -> [Double] -> String
prettyStateAmplitudes _ 0 _ = "No qbits"
prettyStateAmplitudes _ _ [] = ""
prettyStateAmplitudes i n (x:xs) = prettyBits i n ++ " " ++ bar x 50 ++ " " ++ show x ++ "\n\n" ++ prettyStateAmplitudes (i+1) n xs

prettyState :: Ket -> String
prettyState q = prettyStateAmplitudes 0 (nrQbits q) (amplitudes q)

-- Pretty printer de terminos
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

-- Pretty printer de valores
pp :: Value -> IO ()
pp v = putStrLn $ ppTerm $ quote v
  where
    quote (VC c)        = C c
    quote VOple         = Ople
    quote (VAbs t)      = Abs t
    quote (VInjL v')    = InjL (quote v')
    quote (VInjR v')    = InjR (quote v')
    quote (VPair v1 v2) = Pair (quote v1) (quote v2)
    quote (VQbit i)     = QBit i
