{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Vect where

import Data.Kind (Type)
import GHC.TypeLits
import Data.Proxy

data Foo a = Fo a a a

data Vect (n :: Nat) (a :: Type) where
  VNil :: Vect 0 a
  VCons :: a -> Vect m a -> Vect (m + 1) a

unvcons :: (1 <= n) => Vect n a -> (a, Vect (n - 1) a)
unvcons (VCons x xs) = (x, xs)

deriving instance Eq a => Eq (Vect n a)

vhead :: Vect (n + 1) a -> a
vhead v = x where
  (x, _) = unvcons v

vtail :: Vect (n + 1) a -> Vect n a
vtail v = xs where
  (_, xs) = unvcons v

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

class Vtake (n :: Nat) (m :: Nat) where
  vtake :: n <= m => Vect m a -> Vect n a

instance Vtake 0 m where
  vtake _ = VNil

instance (1 <= n, (n - 1) <= (m - 1), Vtake (n - 1) (m - 1)) => Vtake n m where
  vtake v = VCons x (vtake @(n - 1) @(m - 1) xs)
    where
      (x, xs) = unvcons v
