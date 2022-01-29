{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Result
  where

import           Prelude hiding (Word)

import           Data.Foldable
import           Data.List

import           Control.Monad.State

import           Word

import           Ppr

data ResultState = Correct | WrongSpot | Wrong
  deriving (Show, Eq, Ord)

data ResultCell' a = ResultCell ResultState a
  deriving (Show, Eq, Ord, Functor)

type ResultCell = ResultCell' Char
type Result = Word ResultCell

getCellItem :: ResultCell' a -> a
getCellItem (ResultCell _ x) = x

correctCell :: ResultCell' a -> Bool
correctCell (ResultCell Correct _) = True
correctCell _ = False

correctResult :: Result -> Bool
correctResult = all correctCell

parseResultState :: Char -> ResultState
parseResultState '?' = Wrong
parseResultState '*' = WrongSpot
parseResultState _   = Correct

showResultCell :: ResultCell -> Char
showResultCell (ResultCell Correct c) = c
showResultCell (ResultCell Wrong _) = '?'
showResultCell (ResultCell WrongSpot _) = '*'

showResult :: Result -> String
showResult = toList . fmap showResultCell

parseResult :: Word Char -> String -> Result
parseResult guess resultStr =
  ResultCell
    <$> toWord (map parseResultState resultStr)
    <*> guess

genResult :: Word Char -> Guess -> Result
genResult correct guess =
    ResultCell
      <$> flip evalState startUses (sequenceA (go <$> correct <*> guess))
      <*> guess
  where
    startUses :: [(Char, Int)]
    startUses = getCounts $ toList correct

    useChar :: ResultState -> Char -> State [(Char, Int)] ResultState
    useChar state c = do
      uses <- get
      case lookup c uses of
        Nothing -> pure state
        Just 0 -> pure Wrong
        Just _ -> modify (removeFromCount c) *> pure state

    go :: Char -> Char -> State [(Char, Int)] ResultState
    go correctC guessC
      | correctC == guessC    = useChar Correct guessC
      | guessC `elem` correct = useChar WrongSpot guessC
      | otherwise             = pure Wrong

insertIntoCount :: Eq a => a -> [(a, Int)] -> [(a, Int)]
insertIntoCount c [] = [(c, 1)]
insertIntoCount c ((c', count):rest)
  | c' == c   = (c', count+1) : rest
  | otherwise = (c', count) : insertIntoCount c rest

removeFromCount :: Eq a => a -> [(a, Int)] -> [(a, Int)]
removeFromCount _ [] = []
removeFromCount c ((c', count):rest)
  | c' == c   = (c', count-1):rest
  | otherwise = (c', count) : removeFromCount c rest

getCounts :: Eq a => [a] -> [(a, Int)]
getCounts = foldr insertIntoCount []

getWithState :: ResultState -> Result -> [Char]
getWithState s = map extract . filter predicate . toList
  where
    extract (ResultCell _ c) = c
    predicate (ResultCell s' _) = s' == s

instance Ppr (Result, Guess) where
  ppr (r, g) = renderColorfulString $ mconcat $ toList (go <$> r <*> g)
    where
      go (ResultCell Correct c) _ = greenColor [c]
      go (ResultCell Wrong   _) c = defaultColor [c]
      go (ResultCell WrongSpot _) c = yellowColor [c]

