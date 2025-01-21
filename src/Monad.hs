{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Monad (
  PState(..),
  Error,
  MonadQuantum(..),
  Value(..),
  throwError,
  runQState,
  liftIO,
) where

import AST
import qualified Data.Map               as M
import Control.Monad.State
import Control.Monad.Except
import Qbit
import Data.Matrix (colVector)
import PrettyPrinter (prettyQbits)

type Error = String

data PState = State {
  qbits :: Ket,
  vars :: M.Map Name Value
} deriving (Show)


class (MonadIO m, MonadState PState m, MonadError Error m) => MonadQuantum m where
  logM :: String -> m ()
  logM s = liftIO $ putStrLn s

  getQbits :: m Ket
  getQbits = gets qbits

  setQbits :: Ket -> m ()
  setQbits q = modify (\e -> e {qbits = q})

  lookupVar :: Name -> m Value
  lookupVar n = do
    vs <- gets vars
    case M.lookup n vs of
      Just v -> return v
      Nothing -> throwError $ "Variable not found: " ++ n

  updateVar :: Name -> Value -> m ()
  updateVar n v = modify (\e -> e {vars = M.insert n v (vars e)})

  ppState :: m ()
  ppState = do
      q <- getQbits
      logM $ prettyQbits 0 (nrQbits q) (toList q)

  new :: Int -> m Value
  new b = do q <- getQbits
             let q' = tensor q (fromBit b)
             setQbits q'
             return $ VQbit (nrQbits q)

  meas :: Int -> m Value
  meas i = do q <- getQbits
              (q', r) <- liftIO $ measure q i
              setQbits q'
              case r of
                Just 0 -> do return VZero
                Just 1 -> do return VOne
                Just _ -> throwError "Invalid measure"
                Nothing -> throwError "Variable not found"

  apply :: Gate -> Int -> m ()
  apply u i = do q <- getQbits
                 u' <- liftIO $ extend u (nrQbits q) i
                 let q' = u' * colVector q
                 setQbits $ toVector q'

  swap :: Int -> Int -> m ()
  swap i j = do q <- getQbits
                setQbits (swapQbits i j q)

  apply2 :: Gate -> Int -> Int -> m ()
  apply2 u i j | j == i+1  = do  apply u i
               | i < j     = do  swap j (i+1) >> apply2 u i (i+1) >> swap j (i+1)
               | i > j     = do  swap i j >> apply2 u j i >> swap i j
               | otherwise = throwError "Cant apply binary gate to the same qubit"

type QState = StateT PState (ExceptT Error IO)

instance MonadQuantum QState

runQState :: QState a -> PState -> IO (Either Error (a, PState))
runQState m s = runExceptT $ runStateT m s