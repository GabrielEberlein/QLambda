{-# LANGUAGE PatternSynonyms #-}
module AST where

-- Identificadores de Variable
type Name = String

data Stmt i = Def String i           --  Declarar un nuevo identificador x, def x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)

data AbsType = AVar | APair | ANull
data LetType = LVar | LPair | LNull | LFun

data STerm = 
    SC Const
  | SZero
  | SOne
  | SOple
  | STuple [STerm]
  | SAbs AbsType [Name] STerm
  | SLet LetType [Name] STerm STerm
  | SApp STerm STerm
  | SInjL STerm
  | SInjR STerm
  | SIf STerm STerm STerm
  | SMatch STerm Name STerm Name STerm

data Term =
    C Const
  | Ople 
  | Var Name
  | Abs Name Term
  | App Term Term
  | Pair Term Term
  | Let Name Name Term Term
  | InjL Term
  | InjR Term
  | Match Term Name Term Name Term
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
  | VAbs Name Term
  | VInjL Value
  | VInjR Value
  | VOple
  | VPair Value Value
  deriving (Show, Eq)

pattern VZero :: Value
pattern VZero = VInjL VOple

pattern VOne :: Value
pattern VOne = VInjR VOple
