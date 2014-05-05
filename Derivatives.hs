{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------------------
-- |
-- Module : Main
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Derivatives where

--------------------------------------------------------------------------------
import Data.Function

--------------------------------------------------------------------------------
import           Data.Set (Set)
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- | Infix Operators
--------------------------------------------------------------------------------
infixr 3 <~>
infixr 1 <|>
infix 2 ==>, ==>|

--------------------------------------------------------------------------------
-- | Data Declarations
--------------------------------------------------------------------------------
data Token c v
    = Token
      { tokenClass :: c
      , tokenValue :: v
      } deriving (Eq, Ord)

--------------------------------------------------------------------------------
data Ops p c v a where
    Alt :: Ord a => p c v a -> p c v a -> Ops p c v a
    Con :: (Ord a, Ord b) => p c v a -> p c v b -> Ops p c v (a, b)
    Nul :: p c v a -> Ops p c v a
    Eps :: Set a -> Ops p c v a
    Ter :: Set c -> Ops p c v v
    Emp :: Ops p c v a
    Red :: (Set a -> Set b) -> p c v a -> Ops p c v b

--------------------------------------------------------------------------------
data Parser c v a = Parser { parserOps :: Ops Parser c v a }

--------------------------------------------------------------------------------
-- | Smart Constructor
--------------------------------------------------------------------------------
parser :: Ord c => Ops Parser c v a -> Parser c v a
parser = Parser

--------------------------------------------------------------------------------
-- | Combinators
--------------------------------------------------------------------------------
(<|>) :: (Ord a, Ord c) => Parser c v a -> Parser c v a -> Parser c v a
(<|>) p1 p2 = parser $ Alt p1 p2

--------------------------------------------------------------------------------
(<~>) :: (Ord a, Ord b, Ord c)
      => Parser c v a
      -> Parser c v b
      -> Parser c v (a, b)
(<~>) p1 p2 = parser $ Con p1 p2

--------------------------------------------------------------------------------
(==>) :: (Ord b, Ord c) => Parser c v a -> (a -> b) -> Parser c v b
(==>) p f = p ==>| (S.map f)

--------------------------------------------------------------------------------
(==>|) :: Ord c => Parser c v a -> (Set a -> Set b) -> Parser c v b
(==>|) p f = parser $ Red f p

--------------------------------------------------------------------------------
term :: Ord c => c -> Parser c v v
term = parser . Ter . S.singleton

--------------------------------------------------------------------------------
nul :: Ord c => Parser c v a -> Parser c v a
nul = parser . Nul

--------------------------------------------------------------------------------
emp :: Ord c => Parser c v a
emp = parser Emp

--------------------------------------------------------------------------------
eps :: Ord c => Set a -> Parser c v a
eps = parser . Eps

--------------------------------------------------------------------------------
derive :: (Ord c, Eq c) => Parser c v a -> Token c v -> Parser c v a
derive (Parser i) = go i where
  go (Alt p1 p2) t = derive p1 t <|> derive p2 t
  go (Con p1 p2) t = derive p1 t <~> p2 <|> nul p1 <~> derive p2 t
  go (Nul _)     t = emp
  go (Eps _)     t = emp
  go (Ter s)     (Token c v)
      | S.member c s = eps $ S.singleton v
      | otherwise    = emp
  go Emp        _ = emp
  go (Red f p)  t = derive p t ==>| f

--------------------------------------------------------------------------------
parseFull :: Ord c => Parser c v a -> [Token c v] -> Set a
parseFull (Parser Emp)  _ = S.empty
parseFull p            [] = parseNull p
parseFull p (t:ts)        = parseFull (derive p t) ts

--------------------------------------------------------------------------------
parseNull :: Ord c => Parser c v a -> Set a
parseNull = go . nul where
  go (Parser (Eps xs)) = xs
  go (Parser Emp)      = S.empty
  go p                 = go $ compact p

--------------------------------------------------------------------------------
compact :: Ord c => Parser c v a -> Parser c v a
compact p@(Parser i) = go i where
  go (Alt (Parser Emp) (Parser Emp))
      = emp
  go (Alt (Parser Emp) b)
      = compact b
  go (Alt a (Parser Emp))
      = compact a
  go (Alt (Parser (Eps a)) (Parser (Eps b)))
      = eps (S.union a b)
  go (Alt a b)
      = (compact a <|> compact b)
  go (Con (Parser Emp) _)
      = emp
  go (Con _ (Parser Emp))
      = emp
  go (Con (Parser (Eps sM)) b)
      = compact b ==>| (\ xM -> S.fromList [ (s, x) | s <- S.toList sM, x <- S.toList xM ])
  go (Con a (Parser (Eps sM)))
      = compact a ==>| (\ xM -> S.fromList [ (x, s) | x <- S.toList xM, s <- S.toList sM ])
  go (Con a b)
      = compact a <~> compact b
  go (Red _ (Parser Emp))
      = emp
  go (Red f (Parser (Eps xs)))
      = eps (f xs)
  go (Red f (Parser (Red g p)))
      = compact p ==>| f . g
  go (Red f p)
      = (compact p ==>| f)
  go (Nul (Parser (Con a b)))
      = nul (compact a) <~> nul (compact b)
  go (Nul (Parser (Alt a b)))
      = nul (compact a) <|> nul (compact b)
  go (Nul (Parser (Red f p)))
      = nul (compact p) ==>| f
  go (Nul p@(Parser (Nul _)))
      = compact p
  go (Nul (Parser (Eps xs)))
      = eps xs
  go (Nul (Parser (Ter _)))
      = emp
  go (Nul (Parser Emp))
      = emp
  go (Ter _)
      = p
  go (Eps xs)
      | S.null xs = emp
      | otherwise = p
  go Emp
      = p
