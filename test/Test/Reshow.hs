module Test.Reshow where

import Test.HUnit

import Reshow

unit_doubleReshow :: Assertion
unit_doubleReshow =
  Just "4.0" @=? reshowAs @Double "4"
