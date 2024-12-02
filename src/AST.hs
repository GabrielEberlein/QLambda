{-# LANGUAGE PatternSynonyms #-}
module AST where

-- Identificadores de Variable
type Name = String

type Program = [Stmt STerm]

data Stmt i = Def String i           --  Declarar un nuevo identificador x, def x = t
    deriving (Show)

data AbsType = AVar | APair | ANull deriving (Show, Eq)
data LetType = LVar | LPair | LNull | LFun deriving (Show, Eq)

data STerm = 
    SC Const
  | SV Name
  | SZero
  | SOne
  | STuple [STerm]
  | SAbs AbsType [Name] STerm
  | SLet LetType [Name] STerm STerm
  | SApp STerm STerm
  | SInjL STerm
  | SInjR STerm
  | SIf STerm STerm STerm
  | SMatch STerm Name STerm Name STerm
  deriving (Show, Eq)

data Term =
    C Const
  | Ople 
  | Bound Int
  | Free Name
  | Abs Term
  | App Term Term
  | Pair Term Term
  | Let Term Term
  | InjL Term
  | InjR Term
  | Match Term Term Term
  deriving (Show, Eq)

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

data Type =
    T
  | Qbit
  | Exp Type
  | Fun Type Type
  | Tens Type Type
  | Sum Type Type
  deriving (Show, Eq)

data N = N | I Int deriving (Show, Eq)
instance Ord N where
  compare N N = EQ
  compare N _ = GT
  compare _ N = LT
  compare (I n) (I m) = compare n m

pattern Bit :: Type
pattern Bit = Sum T T

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
pattern VZero = VInjL VOple

pattern VOne :: Value
pattern VOne = VInjR VOple
