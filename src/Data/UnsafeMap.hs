module Data.UnsafeMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- | Performs normal lookup in a map, but raises an error with supplied message.
unsLookup :: Ord k => String -> k -> Map k v -> v
unsLookup err k m = Map.findWithDefault (error err) k m
