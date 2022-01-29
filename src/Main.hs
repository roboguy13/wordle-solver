{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Prelude hiding (Word)
import           Trie
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

import Debug.Trace

nub' :: Ord a => [a] -> [a]
nub' = toList . S.fromList

data ResultState = Correct | WrongSpot | Wrong
  deriving (Show, Eq, Ord)

data ResultCell' a = ResultCell ResultState a
  deriving (Show, Eq, Ord)

type ResultCell = ResultCell' Char

data Constraint
  = NotInWord Char
  | AtLeast Char Int
  | Exactly Char Int
  | NotAtIx Char Ix
  | AtIx    Char Ix
  deriving (Show, Eq, Ord)

getCellItem :: ResultCell' a -> a
getCellItem (ResultCell _ x) = x

data Ix = I1 | I2 | I3 | I4 | I5 deriving (Eq, Ord, Enum, Show)

data Word a = Word a a a a a deriving (Eq, Ord, Functor, Foldable, Traversable)

ix :: Ix -> Word a -> a
ix I1 (Word x _ _ _ _) = x
ix I2 (Word _ x _ _ _) = x
ix I3 (Word _ _ x _ _) = x
ix I4 (Word _ _ _ x _) = x
ix I5 (Word _ _ _ _ x) = x

ixes :: Word Ix
ixes = toWord [I1 ..]

instance Show a => Show (Word a) where
  show = show . toList

instance Applicative Word where
  pure x = Word x x x x x
  Word f1 f2 f3 f4 f5 <*> Word x1 x2 x3 x4 x5 = Word (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)

type Result = Word ResultCell
type Guess = Word Char

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
parseResult guess resultStr = ResultCell <$> toWord (map parseResultState resultStr) <*> guess

resultToConstraints :: Result -> [Constraint]
resultToConstraints r = nub' $ notInWordCts ++ countCts ++ fold (go <$> r <*> ixes)
  where
    getExactly :: Char -> Maybe Int
    getExactly c = do
      guard (c `elem` getWithState Wrong r)

      let n = length $ filter (==c) (getWithState Correct r <> getWithState WrongSpot r)

      guard (n > 0)
      pure n

    getAtLeast :: Char -> Maybe Int
    getAtLeast c = do
      guard (c `notElem` getWithState Wrong r)

      let n = length $ filter (==c) (getWithState Correct r <> getWithState WrongSpot r)

      guard (n > 0)
      pure n

    rItems :: Word Char
    rItems = fmap getCellItem r

    countCts :: [Constraint]
    countCts = catMaybes (toList exactlys ++ toList atLeasts)
      where
        exactlys = fmap (\c -> Exactly c <$> getExactly c) rItems
        atLeasts = fmap (\c -> AtLeast c <$> getAtLeast c) rItems

    notInWordCts :: [Constraint]
    notInWordCts = map NotInWord notInWords

    notInWords :: [Char]
    notInWords = mapMaybe go' (nub' (toList r)) \\ inWords
      where
        go' (ResultCell Wrong c) = Just c
        go' _ = Nothing

    inWords :: [Char]
    inWords = mapMaybe go' (toList r)
      where
        go' (ResultCell Wrong _) = Nothing
        go' x = Just $ getCellItem x

    go :: ResultCell -> Ix -> [Constraint]
    go (ResultCell Correct c) i = pure $ AtIx c i
    go (ResultCell Wrong c)   _ = [] --pure $ NotInWord c
    go (ResultCell WrongSpot c) i = pure $ NotAtIx c i

getWithState :: ResultState -> Result -> [Char]
getWithState s = map extract . filter predicate . toList
  where
    extract (ResultCell _ c) = c
    predicate (ResultCell s' _) = s' == s

getNotInWord :: Result -> [Char]
getNotInWord x = getWithState Wrong x \\ (getWithState Correct x ++ getWithState WrongSpot x)

getWrongSpot :: Result -> [Char]
getWrongSpot = getWithState WrongSpot

insertIntoCount :: Eq a => a -> [(a, Int)] -> [(a, Int)]
insertIntoCount c [] = [(c, 1)]
insertIntoCount c ((c', count):rest)
  | c' == c = (c', count+1) : rest
  | otherwise = (c', count) : insertIntoCount c rest

removeFromCount :: Eq a => a -> [(a, Int)] -> [(a, Int)]
removeFromCount _ [] = []
removeFromCount c ((c', count):rest)
  | c' == c =
      -- if count == 0
      --   then error "Tried to remove 0 count character"
        (c', count-1):rest
  | otherwise = (c', count) : removeFromCount c rest

getMinLetterCount :: Result -> [(Char, Int)]
getMinLetterCount = foldr insertIntoCount [] . getChars
  where
    getChars = getWrongSpot <> getWithState Correct

getCount :: Eq a => a -> [a] -> Int
getCount x = length . filter (==x)

getCounts :: Eq a => [a] -> [(a, Int)]
getCounts = foldr insertIntoCount []

correctCount :: String -> [(Char, Int)] -> Bool
correctCount str = all go
  where
    go (c, i) = getCount c str >= i

correctCount' :: Guess -> Result -> Bool
correctCount' g = correctCount (toList g) . getMinLetterCount

-- TODO: Restructure to improve efficiency
matches :: Result -> Guess -> Bool
matches r g = all (matchesConstraint g) (resultToConstraints r)
-- matches r g = and (go <$> r <*> g) && ((notInWord \\ toList g) == notInWord) && correctCount' g r -- && ((wrongSpot \\ toList g) == [])
--   where
--     notInWord = getNotInWord r
--     wrongSpot = getWrongSpot r

--     go (ResultCell Correct c) guessC = guessC == c
--     go (ResultCell WrongSpot c) guessC = guessC /= c
--     go (ResultCell Wrong c) guessC = guessC /= c

matchesConstraint :: Guess -> Constraint -> Bool
matchesConstraint g (NotInWord c) = c `notElem` g
matchesConstraint g (AtLeast c n) = length (filter (== c) (toList g)) >= n
matchesConstraint g (Exactly c n) = length (filter (== c) (toList g)) == n
matchesConstraint g (NotAtIx c i) = ix i g /= c
matchesConstraint g (AtIx    c i) = ix i g == c

type WordTrie = Trie Char

genResult :: Word Char -> Guess -> Result
-- genResult correct guess = makeCell <$> fmap (`evalState` startUses) (go <$> correct <*> guess) <*> guess
genResult correct guess = makeCell <$> (`evalState` startUses) (sequenceA (go <$> correct <*> guess)) <*> guess
  where
    startUses :: [(Char, Int)]
    startUses = getCounts $ toList correct

    makeCell :: ResultState -> Char -> ResultCell
    makeCell = ResultCell

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

correctCell :: ResultCell' a -> Bool
correctCell (ResultCell Correct _) = True
correctCell _ = False

correctResult :: Result -> Bool
correctResult = all correctCell

-- | Precondition: Argument is exactly 5 characters long
toWord :: [a] -> Word a
toWord [a, b, c, d, e] = Word a b c d e
toWord str = error $ "toWord: Incorrect length. Expected 5, got length of " ++ show (length str)

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

showResultPairColors :: (Result, Guess) -> String
showResultPairColors (r, g) = concat $ toList $ go <$> r <*> g
  where
    reset = "\ESC[0m"
    blackText = "\ESC[30m"
    go (ResultCell Correct c) _ = "\ESC[42m" ++ blackText ++ [c] ++ reset
    go (ResultCell Wrong   _) c = [c]
    go (ResultCell WrongSpot _) c = "\ESC[43m" ++ blackText ++ [c] ++ reset

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

      -- let possibleAnswers = buildTrie 0 possibleAnswerList
      --     possibleGuesses = buildTrie 0 possibleGuessList

      gen <- getStdGen
      -- let gen = StdGen {unStdGen = read "SMGen 14993497723278503808 4078047727160935881"}
      putStrLn $ "Random generator state: " ++ show gen

      let initial0 = nub' (possibleAnswerList <> possibleGuessList)
          initial = shuffle' initial0 (length initial0) gen


      let theGuesses = take 6 $ generateGuessList word (map toWord initial)

      mapM_ (putStrLn . showResultPairColors) theGuesses

      -- print $ possibleAnswers
      -- print $ length possibleAnswerList
      -- print $ length possibleGuessList
    _ -> error "Wrong number of arguments. Expected 1"

