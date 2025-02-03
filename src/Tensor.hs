module Tensor (
    Tensor(..)
) where

import Data.Matrix as M
import Data.Vector as V

class Tensor t where
    tensor :: Num a => t a -> t a -> t a

instance Tensor M.Matrix where
    tensor m1 m2 | nrows m1 == 0 || ncols m1 == 0 = m2
                 | nrows m2 == 0 || ncols m2 == 0 = m1
                 | otherwise = (*) <$> m1 <*> m2

instance Tensor V.Vector where
    tensor v1 v2 = V.fromList $ M.toList $ colVector v1 `tensor` colVector v2