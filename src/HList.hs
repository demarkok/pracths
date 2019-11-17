module HList where

import Data.Kind (Constraint, Type)

import Nat
import Vec


data HList (as :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

foo :: HList [Int, String, Bool]
foo = HCons 10 $ HCons "hello" $ HCons True $ HNil

hlength :: Num n => HList as -> n
hlength = go 0
  where
    go :: Num n => n -> HList bs -> n
    go a HNil = a
    go a (HCons _ xs) = go (a + 1) xs


type family All (c :: k -> Constraint) (as :: [k]) :: Constraint where
  All _ '[] = ()
  All c (a ': as) = (c a, All c as)

instance All Show xs => Show (HList xs) where
  show xs = "[" <> go xs
    where
      go :: All Show ys => HList ys -> String
      go HNil = "]"
      go (HCons y HNil) = show y <> "]"
      go (HCons y ys) = show y <> "," <> go ys


type ShowAndNum t = (Show t, Num t)

showZeroAs :: forall t. ShowAndNum t => String
showZeroAs = show (0 :: t)


hhead :: HList (a ': as) -> a
hhead (HCons x _) = x

htail :: HList (a ': as) -> HList as
htail (HCons _ xs) = xs


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

type family Replicate (n :: Nat) (a :: k) :: [k] where
  Replicate 'Zero _ = '[]
  Replicate ('Succ n) a = a ': Replicate n a

vec2hlist :: Vec n a -> HList (Replicate n a)
vec2hlist VNil = HNil
vec2hlist (VCons x xs) = HCons x (vec2hlist xs)


type family Len (as :: [k]) :: Nat where
  Len '[] = 'Zero
  Len (x ': xs) = 'Succ (Len xs)

hlist2vec :: All ((~) a) as => HList as -> Vec (Len as) a
hlist2vec HNil = VNil
hlist2vec (HCons x xs) = VCons x (hlist2vec xs)

class HList2Vec' (n :: Nat) where
  hlist2vec' :: HList (Replicate n a) -> Vec n a

instance HList2Vec' 'Zero where
  hlist2vec' HNil = VNil

instance HList2Vec' n => HList2Vec' ('Succ n) where
  hlist2vec' (HCons x xs) = VCons x (hlist2vec' xs)


hlist2list :: All ((~) a) as => HList as -> [a]
hlist2list = vec2list . hlist2vec

class List2HList (n :: Nat) where
  list2hlist :: [a] -> Maybe (HList (Replicate n a))

instance List2HList 'Zero where
  list2hlist _ = Just HNil

instance List2HList n => List2HList ('Succ n) where
  list2hlist (x : xs) = HCons x <$> list2hlist @n xs
  list2hlist _ = Nothing


type family Map (f :: Type) (as :: [k1]) :: [k2] where
  Map _ '[] = '[]
  Map f (a ': as) = MapType f a ': Map f as



class MapFunction (f :: Type) (a :: Type) where
  type MapType f a :: Type
  mapVal :: a -> MapType f a

data Shower

instance MapFunction Shower Int where
  type MapType Shower Int = String
  mapVal = show

instance MapFunction Shower String where
  type MapType Shower String = String
  mapVal = id

instance MapFunction Shower Bool where
  type MapType Shower Bool = Char
  mapVal False = 'N'
  mapVal True = 'Y'


class HMap (f :: Type) (as :: [Type]) where
  hmap :: HList as -> HList (Map f as)

instance HMap f '[] where
  hmap HNil = HNil

instance (MapFunction f a, HMap f as) => HMap f (a ': as) where
  hmap (HCons x xs) = HCons (mapVal @f x) (hmap @f xs)


class AssocType (f :: Type) where
  type FType f :: Type
type ToMonoid f = (AssocType f, Monoid (FType f))

class ToMonoid f => MapFunctionToMonoid f a where
  mapValToMonoid :: a -> FType f

class AssocType f => HFoldMap (f :: Type) (as :: [Type]) where
  hfoldMap :: HList as -> FType f

instance ToMonoid f => HFoldMap f '[] where
  hfoldMap HNil = mempty

instance (MapFunctionToMonoid f a, HFoldMap f as) => HFoldMap f (a ': as) where
  hfoldMap (HCons x xs) = mapValToMonoid @f x <> hfoldMap @f @as xs


instance AssocType Shower where
  type FType Shower = String

instance Show a => MapFunctionToMonoid Shower a where
  mapValToMonoid = show
