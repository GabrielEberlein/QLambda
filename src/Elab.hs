{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Elab where
import AST
import Data.List (elemIndex)
import Monad (MonadQuantum, liftIO)

type Env = [String]

elab :: MonadQuantum m => Env -> STerm -> m Term
elab _ (SC c) = return $ C c
elab e (SV n) = return $ maybe (Free n) Bound (n `elemIndex` e)
elab _  SZero = return $ InjR Ople
elab _  SOne  = return $ InjL Ople
elab _ (STuple []) = return Ople
elab e (STuple [x]) = elab e x
elab e (STuple (x:xs)) = do p1 <- elab e x
                            p2 <- elab e $ STuple xs 
                            return $ Pair p1 p2
elab e (SAbs AVar [x] m) = do m' <- elab (x:e) m
                              return $ Abs m'
elab e (SAbs AVar (x:xs) m) = do m' <- elab (x:e) $ SAbs AVar xs m
                                 return $ Abs m'
elab e (SAbs APair [x,y] m) = do m' <- elab ([y,x,"*"] ++ e) m
                                 return $ Abs (Let (Bound 0) m')
elab e (SLet LVar [x] m n) = do m' <- elab e m
                                n' <- elab (x:e) n
                                return $ App (Abs n') m'
elab e (SLet LPair [v1,v2] m n) = do m' <- elab e m
                                     n' <- elab ([v2,v1] ++ e) n
                                     return $ Let m' n'
elab e (SLet LFun (f:ys) m n) = elab e $ SLet LVar [f] (SAbs AVar ys m) n
elab e (SApp m n) = do m' <- elab e m
                       n' <- elab e n
                       return $ App m' n'
elab e (SInjL t) = InjL <$> elab e t
elab e (SInjR t) = InjR <$> elab e t
elab e (SPrint s t) = Print s <$> elab e t
elab e (SIf c m n) = do c' <- elab e c
                        m' <- elab ("*":e) m
                        n' <- elab ("*":e) n
                        return $ Match c' m' n'
elab e (SMatch c x m y n) = do c' <- elab e c
                               m' <- elab (x:e) m
                               n' <- elab (y:e) n
                               return $ Match c' m' n'

elabProgram :: MonadQuantum m => [Stmt STerm] -> m [Stmt Term]
elabProgram [] = return []
elabProgram (Def x t:xs) = do t' <- elab [] t
                              xs' <- elabProgram xs
                              return $ Def x t' : xs'
