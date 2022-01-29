{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Word where

import           Prelude hiding (Word)

import           Data.Foldable
import           Control.Applicative

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

ixed :: Word a -> Word (Ix, a)
ixed = liftA2 (,) ixes

instance Show a => Show (Word a) where
  show = show . toList

instance Applicative Word where
  pure x = Word x x x x x
  Word f1 f2 f3 f4 f5 <*> Word x1 x2 x3 x4 x5 = Word (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)

type Guess = Word Char

-- | Precondition: Argument is exactly 5 characters long
toWord :: [a] -> Word a
toWord [a, b, c, d, e] = Word a b c d e
toWord str = error $ "toWord: Incorrect length. Expected 5, got length of " ++ show (length str)

