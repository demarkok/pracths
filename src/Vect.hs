{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Vect where

import Data.Kind (Constraint, Type)
import GHC.TypeLits
import Data.Proxy

data Foo a = Fo a a a

data Vect (n :: Nat) (a :: Type) where
  VNil :: Vect 0 a
  VCons :: a -> Vect m a -> Vect (m + 1) a

vhead :: Vect (n + 1) a -> a
vhead (VCons x _) = x

vtail :: Vect (n + 1) a -> Vect n a
vtail (VCons _ v) = v

vappend :: Vect m a -> Vect n a -> Vect (m + n) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (xs `vappend` ys)

instance (KnownNat n, Show a) => Show (Vect n a) where
  show v = "Vect{" <> show (natVal (Proxy :: Proxy n)) <> "}[" <> go v
    where
      go :: Vect m a -> String
      go VNil = "]"
      go (VCons x VNil) = show x <> "]"
      go (VCons x xs) = show x <> "," <> go xs


-- type family Leq (n :: Nat) (m :: Nat) :: Constraint where
--   Leq n m = ()
--   Leq n m = (CmpNat n m) ~ 'LT

-- class Vtake (n :: Nat) (m :: Nat) where
--   vtake :: Leq n m => Vect m a -> Vect n a

-- instance Vtake 0 m where
--   vtake _ = VNil

-- instance Vtake n m => Vtake (n + 1) (m + 1) where
--   vtake (VCons x xs) = VCons x (vtake xs)
