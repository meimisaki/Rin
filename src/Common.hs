module Common where

import qualified Data.Map as M
import Text.PrettyPrint

type Name = String

anonym :: Name
anonym = ""

extend :: (Ord k, Foldable t) => M.Map k v -> t (k, v) -> M.Map k v
extend = foldr (uncurry M.insert)

accum :: (Monoid a, Functor t, Foldable t) => t (a, b) -> (a, t b)
accum xs = (foldr mappend mempty (fmap fst xs), fmap snd xs)

class Pretty a where
  pprint :: a -> Doc

tab :: Doc -> Doc
tab = nest 2
