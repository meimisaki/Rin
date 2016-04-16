{-# LANGUAGE FlexibleInstances #-}

module Common where

import qualified Data.Map as M
import Text.PrettyPrint

type Name = String

anonym :: Name
anonym = ""

extend :: (Ord k, Foldable t) => M.Map k v -> t (k, v) -> M.Map k v
extend = foldr (uncurry M.insert)

accum :: (Monoid a, Functor t, Foldable t) => t (a, b) -> (a, t b)
accum xs = (foldMap fst xs, fmap snd xs)

class Pretty a where
  pprint :: a -> Doc
  precOf :: a -> Int
  pprintPrec :: Int -> a -> Doc
  {-# MINIMAL pprint #-}
  precOf _ = maxBound
  pprintPrec pr x
    | pr >= precOf x = parens doc
    | otherwise = doc
    where doc = pprint x

instance Pretty Doc where
  pprint = id

instance Pretty Name where
  pprint = text

instance Pretty a => Pretty (Maybe a) where
  pprint = maybe empty pprint
  precOf = maybe maxBound precOf

tab :: Doc -> Doc
tab = nest 2

commaSep :: Pretty a => [a] -> Doc
commaSep = sep . punctuate comma . map pprint
