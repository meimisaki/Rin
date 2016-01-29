module Parser.Combinator
( (|>)
, (<|)
, block
, block1
, imany
, imany1
, imany_
, imany1_
, isepBy
, isepBy1
, isepBy_
, isepBy1_
, ioption
) where

import Parser.Indent

infixl 4 |>, <|

(|>) :: Indent a -> Indent b -> Indent b
ip1 |> ip2 = ip1 *> indented ip2

(<|) :: Indent a -> Indent b -> Indent a
ip1 <| ip2 = ip1 <* indented ip2

block :: Indent a -> Indent [a]
block = withPos . many . aligned

block1 :: Indent a -> Indent [a]
block1 = withPos . many1 . aligned

-- we use the following naming convention for indented combinators
-- prefix `i` stands for `indented`
-- postfix `_` leaves the first token unchecked
imany :: Indent a -> Indent [a]
imany = many . indented

imany1 :: Indent a -> Indent [a]
imany1 = many1 . indented

imany_ :: Indent a -> Indent [a]
imany_ ip = imany1_ ip <|> return []

imany1_ :: Indent a -> Indent [a]
imany1_ ip = (:) <$> ip <*> imany ip

isepBy :: Indent a -> Indent b -> Indent [a]
isepBy ip sep = sepBy (indented ip) (indented sep)

isepBy1 :: Indent a -> Indent b -> Indent [a]
isepBy1 ip sep = sepBy1 (indented ip) (indented sep)

isepBy_ :: Indent a -> Indent b -> Indent [a]
isepBy_ ip sep = isepBy1_ ip sep <|> return []

isepBy1_ :: Indent a -> Indent b -> Indent [a]
isepBy1_ ip sep = (:) <$> ip <*> imany (sep |> ip)

ioption :: a -> Indent a -> Indent a
ioption x = option x . indented
