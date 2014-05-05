--------------------------------------------------------------------------------
-- |
-- Module : Examples
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Examples where

--------------------------------------------------------------------------------
import Derivatives

--------------------------------------------------------------------------------
data Tok
    = Lit Int
    | Op Char deriving (Eq, Ord)

--------------------------------------------------------------------------------
data Exp
    = Value Int
    | Add Exp Exp deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Grammar
--------------------------------------------------------------------------------
parseExp :: Parser String Tok Exp
parseExp = parseAdd <|> parseValue

--------------------------------------------------------------------------------
parseValue :: Parser String Tok Exp
parseValue = term "num" ==> \(Lit i) -> Value i

--------------------------------------------------------------------------------
parseAdd :: Parser String Tok Exp
parseAdd
    = parseExp <~>
      term "+" <~>
      parseExp ==> (\(a, ((Op '+'), b)) -> Add a b)

--------------------------------------------------------------------------------
input :: [Token String Tok]
input = [Token "num" (Lit 1), Token "+" (Op '+'), Token "num" (Lit 2)]

--------------------------------------------------------------------------------
-- | Run
--------------------------------------------------------------------------------
result = parseFull parseExp input -- [Add (Value 1) (Value 2)]
