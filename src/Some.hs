module Some where

import Type.Reflection ((:~~:) (HRefl), Typeable, eqTypeRep, typeOf, typeRep)

data Some' = forall a. MkSome' a

data Some c where
  MkSome :: c a => a -> Some c


showSome :: Some Show -> String
showSome (MkSome x) = show x


type ShowAndNum t = (Show t, Num t)

class ShowAndNum t => ShowAndNumClass t
instance ShowAndNum t => ShowAndNumClass t

showableList :: [Some ShowAndNumClass]
showableList = [MkSome (1 :: Int), MkSome (2 :: Integer)]



data Variant where
  MkVariant :: Typeable a => a -> Variant

showVariantType :: Variant -> String
showVariantType (MkVariant x) = show (typeOf x)

fromVariant :: forall b. Typeable b => Variant -> Maybe b
fromVariant (MkVariant x) =
  case typeOf x `eqTypeRep` typeRep @b of
    Just HRefl -> Just x
    Nothing -> Nothing
--fmap (\HRefl -> x) (typeOf x `eqTypeRep` typeRep @b)


{-
data (:~~:) (a :: Type) (b :: Type) where
  HRefl :: c :~~: c
-}


type Keyable t = (Eq t, Typeable t)

data Entry where
  MkEntry :: Keyable a => a -> a -> Entry

type PolyMap = [Entry]

empty :: PolyMap
empty = []

insert :: Keyable a => a -> a -> PolyMap -> PolyMap
insert key value m = (MkEntry key value) : m

lookup :: Keyable a => a -> PolyMap -> Maybe a
lookup _ [] = Nothing
lookup key ((MkEntry key' v) : m) =
  let nxt = Some.lookup key m in
    case typeOf key `eqTypeRep` typeOf key' of
      Just HRefl -> if key == key' then Just v else nxt
      Nothing -> nxt
