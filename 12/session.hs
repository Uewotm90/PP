{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
-- ghc -I. --make Parsing.hs
import Parsing

data Onion = Core Integer | Layer Onion deriving (Show)

onion :: Parser Onion
onion =
  do
    y <- char 'L'
    rest <- onion
    return (Layer rest)
    <|> do
      c <- core
      return (Core c)

core :: Parser Integer
core = toInteger <$> int

theonion :: String -> Either String Onion
theonion inp = case (parse onion inp) of
  [(o, "")] -> Right o
  [(_, out)] -> Left out
  [] -> Left "invalid input"

type AB = String

ab :: Parser AB
ab =
  do
    a <- char 'a'
    b <- char 'b'
    return (a : [b])
    <|> do
      a <- char 'a'
      mid <- ab
      b <- char 'b'
      return ([a] ++ mid ++ [b])