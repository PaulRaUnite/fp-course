module BinaryTrees where

import           Data.Maybe (fromMaybe)
import           Prelude    hiding ((<=), (^))

data BTree a
  = Empty
  | Branch a
           (BTree a)
           (BTree a)
  deriving (Show, Eq)

depth :: BTree a -> Int
depth Empty                     = 0
depth (Branch value left right) = 1 + max (depth left) (depth right)

leftmost :: BTree a -> Maybe a
leftmost Empty                 = Nothing
leftmost (Branch v left right) = Just $ fromMaybe v $ leftmost left

treewalkDepth :: BTree a -> [a]
treewalkDepth Empty = []
treewalkDepth (Branch value left right) = value : treewalkDepth left ++ treewalkDepth right

treewalkWidth :: BTree a -> [a]
treewalkWidth Empty = []
treewalkWidth x = loop [x]
  where
    loop []                = []
    loop (Empty:xs)        = loop xs
    loop (Branch v l r:xs) = v : loop (xs ++ [l, r])

chopTree :: BTree a -> Int -> BTree a
chopTree Empty _ = Empty
chopTree _ 0 = Empty
chopTree (Branch value left right) level = Branch value (chopTree left (level - 1)) (chopTree right (level - 1))

reflect :: BTree a -> BTree a
reflect Empty                     = Empty
reflect (Branch value left right) = Branch value right left

isSymmetric :: Eq a => BTree a -> Bool
isSymmetric tree = tree == reflect tree

equalUnordered :: Eq a => BTree a -> BTree a -> Bool
equalUnordered Empty Empty = True
equalUnordered Empty _ = False
equalUnordered _ Empty = False
equalUnordered (Branch v1 left1 right1) (Branch v2 left2 right2) =
  v1 == v2 &&
  (equalUnordered left1 left2 && equalUnordered right1 right2)||
   equalUnordered left1 right2 && equalUnordered right1 left2)

(<=) :: BTree a -> BTree a -> BTree a
Empty <= _ = Empty
t <= Empty = t
(Branch _ left1 right1) <= (Branch value left2 right2) = Branch value (left1 <= left2) (right1 <= right2)

(^) :: BTree a -> BTree a -> BTree a
Empty ^ tree = tree
(Branch value left right) ^ tree = Branch value (left ^ tree) (right ^ tree)
