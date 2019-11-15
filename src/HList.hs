module HList where

import Data.Kind (Constraint, Type)


data HList (as :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

foo :: HList '[Int, String, Bool]
foo = HCons 10 $ HCons "hello" $ HCons True $ HNil

hlength :: Num n => HList as -> n
hlength = go 0
  where
    go :: Num n => n -> HList bs -> n
    go a HNil = a
    go a (HCons _ xs) = go (a + 1) xs


type family AllShow (as :: [Type]) :: Constraint where
  AllShow '[] = ()
  AllShow (a ': as) = (Show a, AllShow as)

instance AllShow xs => Show (HList xs) where
  show xs = "[" <> go xs
    where
      go :: AllShow ys => HList ys -> String
      go HNil = "]"
      go (HCons y HNil) = show y<>"]"
      go (HCons y ys) = show y<>","<>go ys


type ShowAndNum t = (Show t, Num t)

showZeroAs :: forall t. ShowAndNum t => String
showZeroAs = show (0 :: t)


hhead :: HList (a ': as) -> a
hhead (HCons x _) = x


type family ReverseGo (acc :: [k]) (as :: [k]) :: [k] where
  ReverseGo acc '[] = acc
  ReverseGo acc (t ': ts) = ReverseGo (t ': acc) ts

type family Reverse (as :: [k]) :: [k] where
  Reverse ts = ReverseGo '[] ts

hreverse :: HList a -> HList (Reverse a)
hreverse = go HNil where
  go :: HList xs -> HList ys -> HList (ReverseGo xs ys)
  go xs HNil = xs
  go xs (HCons y ys) = go (HCons y xs) ys
