{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Eval(
    defEnv,
    evalGate,
    evalConst,
    eval,
    evalProgram
) where

import AST
import Monad
import Qbit
import qualified Data.Map as M
import System.Random (newStdGen)
import Control.Monad.IO.Class (MonadIO)

defEnv :: Env
defEnv = Env {
            qbits = defQBits,
            vars = M.empty
        }

evalGate :: (MonadQuantum m) => Gate -> Value -> m Value
evalGate u v@(VPair (VQbit i) (VQbit j)) = do apply2 u i j
                                              return v
evalGate u v@(VQbit i) = do apply u i
                            return v
evalGate _ _ = throwError "Invalid type"

evalConst :: (MonadQuantum m) => Const -> Value -> m Value
evalConst New VZero = new 0
evalConst New VOne = new 1
evalConst Meas (VQbit i) = meas i
evalConst (U u) v = evalGate u v


eval :: (MonadQuantum m) => Term -> m Value
eval (C t) = return (VC t)
eval Ople = return VOple
eval (Abs t) = return (VAbs t)
eval (Free n) = lookupVar n
eval (Bound _) = throwError "Invalid bound variable"
eval (QBit i) = return (VQbit i)
eval (InjL t) = do v <- eval t
                   return (VInjL v)
eval (InjR t) = do v <- eval t
                   return (VInjR v)
eval (Print s t) = do v <- eval t
                      logM s
                      ppState
                      return v
eval (Pair t1 t2) = do v1 <- eval t1
                       v2 <- eval t2
                       return (VPair v1 v2)
eval (App t1 t2) = do v1 <- eval t1
                      v2 <- eval t2
                      case v1 of
                        VAbs t -> do eval $ subst 0 (quote v2) t
                        VC c -> evalConst c v2
                        _ -> throwError "Invalid application"
eval (Let m n) = do p <- eval m
                    case p of
                      VPair v1 v2 -> do eval $ subst 1 (quote v1) $ subst 0 (quote v2) n
                      _ -> throwError "Invalid let"
eval (Match t l r) = do v <- eval t
                        case v of
                          VInjL v' -> do eval $ subst 0 (quote v') l
                          VInjR v' -> do eval $ subst 0 (quote v') r
                          _ -> throwError "Invalid match"

subst :: Int -> Term -> Term -> Term
subst i t (Bound j) | i == j = t
                    | otherwise = Bound j
subst i t (Abs t') = Abs (subst (i+1) t t')
subst i t (App t1 t2) = App (subst i t t1) (subst i t t2)
subst i t (Let m n) = Let (subst i t m) (subst (i+2) t n)
subst i t (Match c l r) = Match (subst i t c) (subst (i+1) t l) (subst (i+1) t r)
subst i t (Pair t1 t2) = Pair (subst i t t1) (subst i t t2)
subst i t (InjL t') = InjL (subst i t t')
subst i t (InjR t') = InjR (subst i t t')
subst i t (Print s t') = Print s (subst i t t')
subst _ _ t = t

quote :: Value -> Term
quote (VC t) = C t
quote VOple = Ople
quote (VAbs t) = Abs t
quote (VInjL v) = InjL (quote v)
quote (VInjR v) = InjR (quote v)
quote (VPair v1 v2) = Pair (quote v1) (quote v2)
quote (VQbit i) = QBit i



evalProgram :: (MonadQuantum m) => [Stmt Term] -> m Value
evalProgram [] = throwError "Empty program"
evalProgram [Def _ t] = eval t
evalProgram ((Def n t):xs) = do v <- eval t
                                updateVar n v
                                evalProgram xs