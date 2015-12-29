module Common where

import qualified Data.Map as M

type Name = String

extend :: Ord k => M.Map k v -> [(k, v)] -> M.Map k v
extend = foldr (uncurry M.insert)
