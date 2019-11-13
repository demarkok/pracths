module Test.Reshow where

import Test.HUnit

import Data.Proxy
import Reshow

unit_doubleReshow :: Assertion
unit_doubleReshow =
  Just "4.0" @=? reshowAs @Double "4"

unit_doubleReshow' :: Assertion
unit_doubleReshow' =
  Just "4.0" @=? reshowAs' (Proxy :: Proxy Double) "4"
