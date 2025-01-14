{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Qbit (
    Ket,
    fromBit,
    tensor,
    measure,
    extend,
    nrQbits,
    defQBits,
    nrows,
    getGate,
    swapQbits,
    toVector,
    V.toList,
    Complex,
    realPart,
    imagPart,
) where

import AST ( Gate(..) )
import Prelude as P
import Data.Vector as V
import Data.Matrix as M
import Data.Bits ((.&.), Bits (xor))
import Tensor
import Data.Complex
import System.Random
import qualified Control.Applicative as M
import Control.Monad.IO.Class (MonadIO)

type Ket =  Vector (Complex Double)

nrQbits :: Ket -> Int
nrQbits k = floor $ logBase 2 $ fromIntegral $ V.length k

mNrQbits :: Matrix (Complex Double) -> Int
mNrQbits m = floor $ logBase 2 $ fromIntegral $ nrows m

defQBits :: Ket
defQBits = V.singleton (1 :+ 0)

fromBit :: Int -> Ket
fromBit 0 = V.fromList [(1 :+ 0), (0 :+ 0)]
fromBit 1 = V.fromList [(0 :+ 0), (1 :+ 0)]
fromBit _ = error "Invalid bit"

toVector :: Matrix (Complex Double) -> Ket
toVector m = V.fromList $ M.toList m

getGate :: Gate -> Matrix (Complex Double)
getGate X   = M.fromList 2 2 [(0 :+ 0), (1 :+ 0),
                              (1 :+ 0), (0 :+ 0)]

getGate Y   = M.fromList 2 2 [(0 :+ 0), (0 :+ (-1)),
                              (0 :+ 1), (0 :+   0 )]

getGate Z    = M.fromList 2 2 [(1 :+ 0), (  0  :+ 0),
                               (0 :+ 0), ((-1) :+ 0)]

getGate H    = ((1 / sqrt 2) *) <$>
               M.fromList 2 2 [(1 :+ 0), (  1  :+ 0),
                               (1 :+ 0), ((-1) :+ 0)]

getGate CNot = M.fromList 4 4 [(1 :+ 0), (0 :+ 0), (0 :+ 0), (0 :+ 0),
                               (0 :+ 0), (1 :+ 0), (0 :+ 0), (0 :+ 0),
                               (0 :+ 0), (0 :+ 0), (0 :+ 0), (1 :+ 0),
                               (0 :+ 0), (0 :+ 0), (1 :+ 0), (0 :+ 0)]

colapse :: Double -> IO Int
colapse p = do r <- randomRIO (0, 1)
               return $ if r < p then 0 else 1

-- normalize :: Ket -> Ket
-- normalize xs = let p = sqrt $ sum $ map (\x -> x^2) xs in map (\x -> x / p) xs

-- Dado el ket que mantiene el estado de un sistema de
-- n qubits, se mide el qubit en la posición i-ésima.
measure :: Ket -> Int -> IO (Ket, Maybe Int)
measure ket i = do let n = nrQbits ket
                   let ib = 2^(n-i-1)
                   let p0 = P.sum $ [(ket V.! idx)^2 | idx <- [0 .. V.length ket - 1], (idx .&. ib == 0)]
                   r <- colapse (realPart p0)
                   case r of
                        0 -> return (V.imap  (\ idx qb -> (/sqrt p0) (if (idx .&. ib == 0) then qb else 0:+0)) ket, Just 0)
                        1 -> return (V.imap  (\ idx qb -> (/sqrt (1-p0)) (if (idx .&. ib == 0) then 0:+0 else qb)) ket, Just 1)
                        _ -> return (ket, Just r)


-- Función que extiende una matriz a una de mayor dimensión
extend :: Gate -> Int -> Int -> IO (Matrix (Complex Double))
extend u n i = do let g = getGate u
                  let m = M.identity (2^i) `tensor` g `tensor` M.identity (2^(n - i - (mNrQbits g)))
                  return m

swapQbits :: Int -> Int -> Ket -> Ket
swapQbits i j ket | i > j = swapQbits j i ket
                  | otherwise = let n = nrQbits ket
                                    (ib, jb) = (2^(n-i-1), 2^(n-j-1))
                                in V.generate (V.length ket) (\k -> ket V.! (newIndex ib jb k))

newIndex :: Int -> Int -> Int -> Int
newIndex ib jb k = let r = (ib .&. k) `xor` (jb .&. k) in
                    if r == ib then (k - (ib - jb)) else
                    if r == jb then (k + (ib - jb)) else k


