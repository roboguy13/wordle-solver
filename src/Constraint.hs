{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Constraint
  where

import           Prelude hiding (Word)

import           Data.Bifunctor
import           Data.Foldable
import           Data.Coerce
import           Data.List

import           Control.Monad.State

import           Word
import           Result

-- data Defined a = Unknown | Known a deriving (Functor)

newtype Defined a = Defined (Maybe a) deriving (Functor, Applicative, Monad)

pattern Known x = Defined (Just x)
pattern Unknown = Defined Nothing

data Constraint
  = NotInWord Char
  | AtLeast Char Int
  | Exactly Char Int
  | NotAtIx Char Ix
  | AtIx    Char Ix
  deriving (Show, Eq, Ord)

-- getConstraintChar :: Constraint -> Char
-- getConstraint

newtype Occurs a = Occurs [(a, Int)] deriving (Functor)

insertOccurs :: Eq a => a -> Occurs a -> Occurs a
insertOccurs x (Occurs []) = Occurs [(x, 1)]
insertOccurs x (Occurs (y:ys))
  | fst y == x = Occurs (second (+1) y : ys)
  | otherwise = Occurs (y : coerce (insertOccurs x (Occurs ys)))

updateOccurs :: ResultCell -> Occurs Char -> Occurs Char
updateOccurs (ResultCell Wrong _) occurs = occurs
updateOccurs (ResultCell _ c) occurs = insertOccurs c occurs

lookupOccurs :: Eq a => a -> Occurs a -> Maybe Int
lookupOccurs x (Occurs occurs) = lookup x occurs

-- lookupConstraints :: Char -> [Constraint] -> Defined Constraint
-- lookupConstraints _ [] = Unknown
-- lookupConstraint c (

insertResult :: Result -> [Constraint] -> [Constraint]
insertResult r cts0 = (cts0 ++) . toList . flip evalState occurs0 $ mapM go (ixed r)
  where
    occurs0 = foldr updateOccurs (Occurs []) r

    go :: (Ix, ResultCell' Char) -> State (Occurs Char) Constraint
    go arg = do
      occurs <- get
      pure (resultCellToCt occurs arg)

resultCellToCt :: Occurs Char -> (Ix, ResultCell' Char) -> Constraint
resultCellToCt occurs (i, ResultCell Wrong c) =
  case lookupOccurs c occurs of
    Nothing -> NotInWord c
    Just 0 -> NotInWord c
    Just occursC -> Exactly c occursC

resultCellToCt _occurs (i, ResultCell Correct c) = AtIx c i
resultCellToCt _occurs (i, ResultCell WrongSpot c) = NotAtIx c i

matchesConstraint :: Guess -> Constraint -> Bool
matchesConstraint g (NotInWord c) = c `notElem` g
matchesConstraint g (AtLeast c n) = length (filter (== c) (toList g)) >= n
matchesConstraint g (Exactly c n) = length (filter (== c) (toList g)) == n
matchesConstraint g (NotAtIx c i) = ix i g /= c && c `elem` g
matchesConstraint g (AtIx    c i) = ix i g == c

