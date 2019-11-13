module Reshow where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Text.Read

class Reshow (t :: Type) where
  reshowAs :: String -> Maybe String

instance (Read t, Show t) => Reshow t where
  reshowAs s = show <$> (readMaybe s :: Maybe t)

reshowAs' :: forall a. (Read a, Show a) => Proxy a -> String -> Maybe String
reshowAs' p s = show <$> (readMaybe s :: Maybe a)
