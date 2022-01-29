{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trie
  (Trie
  ,buildTrie
  ,insertIntoTrie
  ,isInTrie
  )
  where

import           Data.Monoid

import           Data.Bifunctor

-- | Rotated trie
data Trie a =
  Trie
  { trieRotation :: Int
  , trieRoot :: Node a
  }
  deriving (Show)

rotate :: Int -> [a] -> [a]
rotate i = (!! i) . iterate rotateStep
  where
    rotateStep :: [a] -> [a]
    rotateStep []     = []
    rotateStep (x:xs) = xs ++ [x]

isInTrie :: Ord a => [a] -> Trie a -> Bool
isInTrie xs t = isInNode xs (trieRoot t)

buildTrie :: Ord a => Int -> [[a]] -> Trie a
buildTrie rotation xs =
  Trie rotation $ buildNode (map (rotate rotation) xs)

insertIntoTrie :: Ord a => [a] -> Trie a -> Trie a
insertIntoTrie xs t = t { trieRoot = insertIntoNode (rotate (trieRotation t) xs) (trieRoot t) }

-- | Rose tree-like structure
newtype Node a = Node [(a, Node a)]
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Show)

singletonNode :: [a] -> Node a
singletonNode [] = Node []
singletonNode (x:xs) = Node [(x, singletonNode xs)]

data NodeZipper a =
  NodeZipper
  { nodeZipperPrevious :: Node a
  , nodeZipperCurrent :: (a, Node a)
  , nodeZipperNext :: Node a
  }

reconstructNode :: NodeZipper a -> Node a
reconstructNode (NodeZipper x y z) = x <> Node [y] <> z

onCurrent :: (Node a -> Node a) -> NodeZipper a -> NodeZipper a
onCurrent f (NodeZipper x y z) = NodeZipper x (second f y) z

insertIntoNode :: Ord a => [a] -> Node a -> Node a
insertIntoNode [] t = t
insertIntoNode (x:xs) (Node n) =
  case findAmongNodes x n of
    Left (prev, next) -> prev <> singletonNode (x:xs) <> next

    Right z -> reconstructNode $ onCurrent (insertIntoNode xs) z

buildNode :: Ord a => [[a]] -> Node a
buildNode xs = foldl (flip insertIntoNode) mempty xs

nodeChildren :: Node a -> [(a, Node a)]
nodeChildren (Node xs) = xs

-- -- Precondition: Rose nodes are sorted least to greatest
findAmongNodes :: forall a. Ord a => a -> [(a, Node a)] -> Either (Node a, Node a) (NodeZipper a)
findAmongNodes x = go mempty
  where
    go :: Node a -> [(a, Node a)] -> Either (Node a, Node a) (NodeZipper a)
    go prev [] = Left (prev, mempty)
    go prev next@(curr@(y, n):rest)
      | x <  y = Left (prev, Node next)
      | x == y = Right (NodeZipper prev curr (Node rest))
      | otherwise = go (prev <> Node [curr]) rest


isInNode :: forall a. Ord a => [a] -> Node a -> Bool
isInNode [] (Node []) = True
isInNode [] _ = False
isInNode (x:xs) (Node ns) =
  case nodeZipperCurrent <$> findAmongNodes x ns of
    Left {} -> False
    Right (_, n') -> isInNode xs n'

