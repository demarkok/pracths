module Test.Vect where

import Hedgehog
import Test.HUnit

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Nat
import Vect


unit_vecShow :: Assertion
unit_vecShow =
  "Vect{3}[1,2,3]" @=? show (VCons 1 $ VCons 2 $ VCons (3 :: Int) $ VNil)

-- hprop_vtake3of4 :: Property
-- hprop_vtake3of4 = property $ do
--   let genint = G.int R.linearBounded
--   (a, b, c, d) <- forAll $ (,,,) <$> genint <*> genint <*> genint <*> genint
--   let v = VCons a $ VCons b $ VCons c $ VCons d $ VNil
--   "Vect{3}[" <> show a <> "," <> show b <> "," <> show c <> "]" === show (vtake 3 v)
