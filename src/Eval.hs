{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
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


eval :: (MonadQuantum m) => [Value] -> Term -> m Value
eval _ (C t) = return (VC t)
eval _  Ople = return VOple
eval _ (Abs t) = return (VAbs t)
eval _ (Free n) = lookupVar n
eval e (Bound i) = return (e !! i)
eval e (InjL t) = do v <- eval e t
                     return (VInjL v)
eval e (InjR t) = do v <- eval e t
                     return (VInjR v)
eval e (Pair t1 t2) = do v1 <- eval e t1
                         v2 <- eval e t2
                         return (VPair v1 v2)
eval e (App t1 t2) = do v1 <- eval e t1
                        v2 <- eval e t2
                        case v1 of
                          VAbs t -> do eval (v2:e) t
                          VC c -> evalConst c v2
                          _ -> throwError "Invalid application"
eval e (Let m n) = do p <- eval e m
                      case p of
                        VPair v1 v2 -> do eval ([v2,v1]++e) n
                        _ -> throwError "Invalid let"
eval e (Match t l r) = do v <- eval e t
                          case v of
                            VInjL v' -> do eval (v':e) l
                            VInjR v' -> do eval (v':e) r
                            _ -> throwError "Invalid match"

evalProgram :: (MonadQuantum m) => [Stmt Term] -> m Value
evalProgram [] = throwError "Empty program"
evalProgram [Def _ t] = eval [] t
evalProgram ((Def n t):xs) = do v <- eval [] t
                                updateVar n v
                                evalProgram xs