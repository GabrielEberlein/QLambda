{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Eval(
    defEnv,
    evalGate,
    evalConst,
    eval,
    evalStmt
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
evalGate u _ = throwError "Invalid type"

evalConst :: (MonadQuantum m) => Const -> Value -> m Value
evalConst New VZero = new 0
evalConst New VOne = new 1
evalConst Meas (VQbit i) = meas i
evalConst (U u) v = evalGate u v


eval :: (MonadQuantum m) => Term -> m Value
eval (C t) = return (VC t)
eval (Ople) = return VOple
eval (Abs n t) = return (VAbs n t)
eval (Var n) = lookupVar n
eval (InjL t) = do v <- eval t
                   return (VInjL v)
eval (InjR t) = do v <- eval t
                   return (VInjR v)
eval (Pair t1 t2) = do v1 <- eval t1
                       v2 <- eval t2
                       return (VPair v1 v2)
eval (App t1 t2) = do v1 <- eval t1
                      v2 <- eval t2
                      case v1 of
                        VAbs n t -> do updateVar n v2
                                       eval t
                        VC c -> evalConst c v2
                        _ -> throwError "Invalid application"
eval (Let x y m n) = do p <- eval m
                        case p of
                          VPair v1 v2 -> do updateVar x v1
                                            updateVar y v2
                                            eval n
                          _ -> throwError "Invalid let"
eval (Match t x l y r) = do v <- eval t
                            case v of
                              VInjL v' -> do updateVar x v'
                                             eval l
                              VInjR v' -> do updateVar y v'
                                             eval r
                              _ -> throwError "Invalid match"

evalStmt :: (MonadQuantum m) => [Stmt Term] -> m Value
evalStmt [Def _ t] = eval t
evalStmt ((Def n t):xs) = do v <- eval t
                             updateVar n v
                             evalStmt xs
evalStmt [Eval t] = eval t
evalStmt ((Eval t):xs) = do _ <- eval t
                            evalStmt xs