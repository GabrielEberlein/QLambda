module Typechecker where
import AST
import Monad

-- (<:) :: Type -> Type -> Bool
-- (Exp  n  a) <: (Exp  m  b ) = (m <= n) && a <: b
-- (Exp  _  a) <: b = a <: b
-- (Tens a  b) <: (Tens a' b') =  a <: a' && b <: b'
-- (Sum  a  b) <: (Sum  a' b') =  a <: a' && b <: b'
-- (Fun  a' b) <: (Fun  a  b') =  a <: a' && b <: b' 
-- a <: b = a == b

-- hasType :: (MonadQuantum m) => Term -> Type -> m Bool
-- hasType 
-- hasType t ty = do
--   ty' <- tc t
--   return (ty' <: ty)

-- tc :: (MonadQuantum m) => Term -> m Type
-- tc (C c) = tcConst c
-- tc Ople = return (Exp N T)
-- tc (Abs n ty t) = 

-- tcConst :: (MonadQuantum m) => Const -> m Type
-- tcConst (U u) = tcGate u
-- tcConst New   = return (Fun Bit Qbit)
-- tcConst Meas  = return (Fun Qbit (Exp N Bit))

-- tcGate :: (MonadQuantum m) => Gate -> m Type
-- tcGate X    = return (Fun Qbit Qbit)
-- tcGate Y    = return (Fun Qbit Qbit)
-- tcGate Z    = return (Fun Qbit Qbit)
-- tcGate H    = return (Fun Qbit Qbit)
-- tcGate CNot = return (Fun (Tens Qbit Qbit) (Tens Qbit Qbit))