module Pack1
  ( myLast
  , myButLast
  , elementAt
  , myLength
  , myLength'
  , myReverse
  , myReverse'
  , isPalindrome
  , isPalindrome'
  , NestedList(Elem, List)
  , flatten
  , compress
  , pack
  , encode
  ) where

-- In the module I'll try to define only total functions
-- even if it is possible only by failure at some inputs.
-- Get last element of generic type list.
-- The main idea is to write edge cases and one step rule that
-- after some finite amount of steps will trigger these edge-cases.
myLast :: [a] -> a
-- If the input is an empty list - error.
myLast []     = error "Last element of empty list doesn't exist."
-- If a list has only one element - return it.
myLast [x]    = x
-- If a list has 2 or more elements - split into head (x) and tail (xs)
-- and recursively get last element of tail.
myLast (x:xs) = myLast xs

-- Get element of generic list before last one.
myButLast :: [a] -> a
-- If a list is empty - error.
myButLast [] = error "Previous of last element doesn't exist in an empty list."
-- If a list contains only one element - error.
myButLast [x] = error "Previous of last element doesn't exist in a list of one element."
-- If a list has exactly two elements - return first one.
myButLast [x, _] = x
-- As case of len(list) <= 2 we checked, get but-last
-- element recursively by dropping head element from the list.
myButLast (x:xs) = myButLast xs

-- Returns element at some position or raises error.
elementAt :: [a] -> Int -> a
-- Empty list cannot be indexed.
elementAt [] _ = error "Cannot get any element from an empty list."
-- Basic case of list and first position to return.
elementAt (e:_) 1 = e
-- Recursively try to step though a list
elementAt (_:xs) k
  -- Negative indexing is not supported.
  | k < 1 = error "List doesn't support zero or negative indexing."
  -- Decrease index and try to get element in the next step (recursively).
  | otherwise = elementAt xs (k - 1)

-- Return length of an generic list.
myLength :: [a] -> Int
-- Fold a list from left to right, i.e.:
-- myLength [a,b,c] => 1 + myLength [b, c] => 2 + myLength [c] => 3
myLength = foldl (\acc _ -> acc + 1) 0

-- Second version of length function.
myLength' :: [a] -> Int
-- Transform all elements of a list to list of 1 and sum them all! :D
myLength' = sum . map (const 1)

-- Return reversed list.
myReverse :: [a] -> [a]
-- Fold the list in reverse order.
-- myReverse [1,2,3] == [] ++ head of [1,2,3] => [1] ++ head of [2,3] => ...
myReverse = foldl (flip (:)) []

-- Return reversed list.
myReverse' :: [a] -> [a]
-- The first and single edge-case: a reversed empty list is an empty list.
myReverse' []     = []
-- Recursively reverse tail of the list and append head to it.
myReverse' (x:xs) = myReverse' xs ++ [x]

-- Predicate that checks does a list belongs to palindromes.
-- Palindrome is a symmetric word.
-- Naїve version.
isPalindrome :: (Eq a) => [a] -> Bool
-- If a list is empty - palindrome.
isPalindrome [] = True
-- If a list has only one element - it is symmetric so it is palindrome too.
isPalindrome [_] = True
-- If the first and the last elements of the list are equal and
-- list between them palindrome too - it is palindrome.
isPalindrome list = head list == last list && (isPalindrome . init . tail) list

-- Predicate that checks does a list belongs to palindromes.
-- Palindrome is a symmetric word.
isPalindrome' :: (Eq a) => [a] -> Bool
-- List is palindrome if it equals to the flipped self:
-- empty list: [] == reverse [] => [] == []
-- one-element list: [1] == reverse [1] => [1] == [1]
-- variable length list: [1,2,1] == reverse [1,2,1] => [1,2,1] == [1,2,1]
-- [1,2,3] == reverse [1,2,3] => [1,2,3] != [3,2,1]
isPalindrome' list = list == reverse list

-- Data type for nested list.
data NestedList a
  = Elem a -- Represents list with one element
  | List [NestedList a] -- Set of nested lists.

-- Unwraps or flattens NestedList into plain list.
flatten :: NestedList a -> [a]
flatten list =
  case list of
    Elem e -> [e] -- Wrap element into list.
    List s -> concatMap flatten s -- List of lists recursively flatten and concat each to other.

-- Remove sequential duplicates.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs)
  | null xs = [x]
  -- If head of current list equals to head of tail
  -- means that they are sequential duplicates, so
  -- we drop the first of them and continue recursively.
  | x == head xs = compress xs
  -- If not, leave the element at its place.
  | otherwise = x : compress xs

-- Pack sequential duplicates into sub-lists.
pack :: (Eq a) => [a] -> [[a]]
-- No duplicates in empty list. :D
pack [] = []
pack (x:xs) =
  let subList = x : packBy x xs
  -- Return sub list only with
  -- the element in the tail and add the element to it.
   in subList : pack (drop (length subList - 1) xs)
   -- Concat it to the others:
   -- skip the sub-list length, pack again.
  where
    packBy el [] = []
    -- Returns first elements of list that equals to given.
    packBy el (y:ys)
      | el == y = y : packBy el ys
      -- Or return empty list.
      | otherwise = []

-- Return elements and their sequential frequencies.
encode :: Eq a => [a] -> [(Int, a)]
-- Packs the list and computes length of every sub-list into a tuple.
encode = map (\l -> (length l, head l)) . pack
