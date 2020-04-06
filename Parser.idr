||| This module implements a parser combinator monad, which is used to parse our programs into an AST representation
module Parser


Parser : Type -> Type
Parser a = List Char -> List (a, List Char)

applyParser : (a -> Parser b) -> (a, List Char) -> List (b, List Char)
applyParser f (a, s) = let p = f a in p s

(>>=) : Parser a -> (a -> Parser b) -> Parser b
(>>=) p f = \s => concat $ map (applyParser f) (p s) 

return : a -> Parser a
return a = \s => [(a, s)]

empty : Parser a
empty = \s => []

item : Parser Char
item [] = []
item (x::xs) = [(x, xs)]

sat : (Char -> Bool) -> Parser Char
sat p = do
  c <- item 
  if p c
     then return c
     else empty
