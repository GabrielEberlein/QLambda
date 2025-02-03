{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AST where


type Name = String

type Program = [Decl STerm]

data Decl i = Def String i
    deriving (Show)


-- Tipos de abstraciones azucaradas
data AbsType = AVar | APair | ANull deriving (Show, Eq)
-- Tipos de let azucarados
data LetType = LVar | LPair | LNull | LFun | LRec deriving (Show, Eq)


-- Arbol de sintaxis abstracta con azucar sintactico
data STerm = 
    SC Const
  | SV Name
  | SZero
  | SOne
  | SPrint String STerm
  | STuple [STerm]
  | SAbs AbsType [Name] STerm
  | SLet LetType [Name] STerm STerm
  | SApp STerm STerm
  | SInjL STerm
  | SInjR STerm
  | SIf STerm STerm STerm
  | SMatch STerm Name STerm Name STerm
  deriving (Show, Eq)

-- Arbol de sintaxis abstracta nucleo
data Term =
    C Const
  | Ople 
  | Print String Term
  | QBit Int
  | Bound Int
  | Free Name
  | Abs Term
  | App Term Term
  | Pair Term Term
  | Let Term Term
  | Rec Term Term
  | InjL Term
  | InjR Term
  | Match Term Term Term
  deriving (Show, Eq)

pattern Zero :: Term
pattern Zero = InjR Ople

pattern One :: Term
pattern One = InjL Ople

data Const =
    U Gate
  | New
  | Meas
  deriving (Show, Eq)

data Gate = 
    X
  | Y
  | Z
  | H
  | CNot
  deriving (Show, Eq)

-- Arbol de sintaxis abstracta de valores
data Value =
    VC Const
  | VQbit Int
  | VAbs Term
  | VInjL Value
  | VInjR Value
  | VOple
  | VPair Value Value
  deriving (Show, Eq)

pattern VZero :: Value
pattern VZero = VInjR VOple

pattern VOne :: Value
pattern VOne = VInjL VOple
