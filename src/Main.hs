{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Prelude hiding (Word)
import           Data.List
import           System.Environment

import           Data.Foldable
import           Data.Monoid
import           Data.Ord

import qualified Data.Set as S

import           Control.Monad.State

import           Data.Maybe
import           System.Random
import           System.Random.Shuffle
import           System.Random.Internal
import qualified System.Random.SplitMix as SM

-- import           Trie
import           Word
import           Constraint
import           Result
import           Ppr

import Debug.Trace

nub' :: Ord a => [a] -> [a]
nub' = toList . S.fromList

resultToConstraints :: Result -> [Constraint]
resultToConstraints r = insertResult r []

-- TODO: Restructure to improve efficiency
matches :: Result -> Guess -> Bool
matches r g = all (matchesConstraint g) (resultToConstraints r)

correctCell :: ResultCell' a -> Bool
correctCell (ResultCell Correct _) = True
correctCell _ = False

correctResult :: Result -> Bool
correctResult = all correctCell

wordToString :: Word Char -> String
wordToString = toList

generateGuess :: [Word Char] -> [(Result, Guess)] -> Guess
generateGuess possibleGuesses results0 =
  let results = map fst results0
  in
  case sortOn (Down . getWeight results) $ filter (\g -> all (`matches` g) results) possibleGuesses of
    [] -> error $ "Cannot find guess. This should be unreachable, if the hints are correct. Failed with results:\n" ++ unlines (map showResultPair results0) ++ "\n" ++ show (concatMap resultToConstraints results)
    (guess:_) -> guess

generateGuessList :: Word Char -> [Word Char] -> [(Result, Guess)]
generateGuessList correct possibleGuesses0 = go possibleGuesses0 []
  where
    go :: [Word Char] -> [(Result, Guess)] -> [(Result, Guess)]
    go remainingGuesses results =
      let guess = generateGuess remainingGuesses results
          result = genResult correct guess
          newResults = results ++ [(result, guess)]
      in
        if correctResult result
          then newResults
          else go (delete guess remainingGuesses) newResults

showResultPair :: (Result, Guess) -> String
showResultPair (r, g) = toList g <> "   " <> showResult r

getWeight :: [Result] -> Guess -> Int
getWeight results guess = length (toList guess \\ guessedChars)
  where
    guessedChars = nub' $ concatMap (map getCellItem . toList) results

main :: IO ()
main =
  getArgs >>= \case
    [theWord] -> do
      let word = toWord theWord

      possibleAnswerList <- read <$> readFile "../possible-answers.txt" :: IO [String]
      possibleGuessList <- read <$> readFile "../possible-guesses.txt" :: IO [String]

      gen <- getStdGen
      -- let gen = StdGen {unStdGen = read "SMGen 14993497723278503808 4078047727160935881"}
      putStrLn $ "Random generator state: " ++ show gen

      let initial0 = nub' (possibleAnswerList <> possibleGuessList)
          initial = shuffle' initial0 (length initial0) gen


      let theGuesses = take 6 $ generateGuessList word (map toWord initial)

      mapM_ (putStrLn . ppr) theGuesses

    _ -> error "Wrong number of arguments. Expected 1"

