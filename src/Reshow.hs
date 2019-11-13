module Reshow where

import Data.Kind (Type)
import Text.Read

class Reshow (t :: Type) where
  reshowAs :: String -> Maybe String

instance (Read t, Show t) => Reshow t where
  reshowAs s = show <$> ((readMaybe s) :: Maybe t)
