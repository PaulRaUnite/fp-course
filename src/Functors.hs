{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Functors where

import           Data.List.Split   (splitOn)
import           Data.String.Utils (replace)

import           Prelude           hiding (return)

data BTree a
  = Empty
  | Branch a
           (BTree a)
           (BTree a)

instance Show a => Show (BTree a) where
  show Empty = "*"
  show (Branch v left right) =
    (show v) ++ "\n" ++ lefts ++ replace "\n" ("\n" ++ (concat $ replicate spaces " ")) (show right)
    where
      lefts = show left
      spaces = foldl max 0 (map length $ splitOn "\n" lefts)

instance Eq a => Eq (BTree a) where
  Empty == Empty = True
  (Branch v1 left1 right1) == (Branch v2 left2 right2) = v1 == v2 && left1 == left2 && right1 == right2

instance Functor BTree where
  fmap fn Empty                 = Empty
  fmap fn (Branch v left right) = Branch (fn v) (fn <$> left) (fn <$> right)

instance Applicative BTree where
  pure x = Branch x Empty Empty
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (Branch f leftf rightf) <*> (Branch v left right) = Branch (f v) (leftf <*> left) (rightf <*> right)

data LTree a
  = Leaf a
  | LBranch (LTree a)
            (LTree a)
  deriving (Show)

instance Functor LTree where
  fmap f (Leaf v)             = Leaf $ f v
  fmap f (LBranch left right) = LBranch (fmap f left) (fmap f right)

instance Applicative LTree where
  pure = Leaf
  Leaf f <*> Leaf v = Leaf $ f v
  Leaf f <*> tree@(LBranch left right) = f <$> tree
  LBranch leftf rightf <*> leaf@(Leaf v) = LBranch (leftf <*> leaf) (rightf <*> leaf)
  tree@(LBranch leftf rightf) <*> LBranch left right = LBranch (tree <*> left) (tree <*> right)

-- Kleisli arrow
-- Left identity: return >=> g	= g
-- Right identity:	f >=> return = f
-- Associativity: (f >=> g) >=> h = f >=> (g >=> h)
class Kleisli k where
  (>=>) :: (a -> k b) -> (b -> k c) -> (a -> k c)
  (<=<) :: (b -> k c) -> (a -> k b) -> (a -> k c)
  return :: a -> k a
  (<=<) = flip (>=>)
  (>=>) = flip (<=<)

infixl 9 >=>

infixl 9 <=<

instance Kleisli Maybe where
  return = Just
  f >=> g = lam . f
    where
      lam Nothing  = Nothing
      lam (Just x) = g x

instance Kleisli [] where
  return v = [v]
  f >=> g = foldMap g . f

class MyFunctor k where
  fmap' :: (a -> b) -> k a -> k b

instance (Applicative f) => MyFunctor f where
  fmap' f v = pure f <*> v
--instance (Kleisli k) => MyFunctor k where
--  fmap' f v = f <=< return v
