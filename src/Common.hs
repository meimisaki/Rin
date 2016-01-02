module Common where

import qualified Data.Map as M

type Name = String

extend :: (Ord k, Foldable t) => M.Map k v -> t (k, v) -> M.Map k v
extend = foldr (uncurry M.insert)

accum :: (Monoid a, Functor t, Foldable t) => t (a, b) -> (a, t b)
accum xs = (foldr mappend mempty (fmap fst xs), fmap snd xs)
