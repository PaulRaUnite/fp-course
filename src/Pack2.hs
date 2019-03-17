module Pack2 where

import           Pack1   (encode)
import           Prelude

data SM a
  = Single a
  | Multiple Int
             a
  deriving (Show)

-- Modified run-length encoding.
-- λ> encodeModified "aaaabccaadeeee"
--    [Multiple 4 'a',Single 'b',Multiple 2 'c',
--    Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeModified :: Eq a => [a] -> [SM a]
-- Replacing tuple by data type
encodeModified =
  map
    (\(l, e) ->
       if l == 1
         then Single e
         else Multiple l e) .
  encode

-- Decode a run-length encoded list.
-- λ> decodeModified
--       [Multiple 4 'a',Single 'b',Multiple 2 'c',
--        Multiple 2 'a',Single 'd',Multiple 4 'e']
--    "aaaabccaadeeee"
decodeModified :: [SM a] -> [a]
decodeModified = concatMap unpack
  where
    unpack (Single e)     = [e]
    unpack (Multiple n e) = replicate n e

-- Run-length encoding of a list (direct solution).
-- λ> encodeDirect "aaaabccaadeeee"
--    [Multiple 4 'a',Single 'b',Multiple 2 'c',
--    Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect :: Eq a => [a] -> [SM a]
encodeDirect [] = []
encodeDirect (x:xs)
  | count == 1 = Single x : encodeDirect xs
  | otherwise = Multiple count x : encodeDirect tail
    -- Get longest prefix of the xs tail list
  where
    (prefix, tail) = span (== x) xs
    count = 1 + length prefix

-- Duplicate the elements of a list.
-- λ> dupli [1, 2, 3]
--    [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli = foldl (\acc x -> acc ++ [x, x]) []

-- Replicate the elements of a list a given number of times.
-- λ> repli "abc" 3
--    "aaabbbccc"
repli :: [a] -> Int -> [a]
repli l n = foldl (\acc x -> acc ++ replicate n x) [] l

-- Drop every N'th element from a list.
-- λ> dropEvery "abcdefghik" 3
--    "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery _ 1 = []
dropEvery list n = dropEveryCount list n 1
  where
    dropEveryCount l n c
      | null l = []
      | c `rem` n == 0 = dropEveryCount xs n (c + 1)
      | otherwise = head l : dropEveryCount xs n (c + 1)
      where
        (x:xs) = l

-- Split a list into two parts; the length of the first part is given.
-- λ> split "abcdefghik" 3
--    ("abc", "defghik")
split :: [a] -> Int -> ([a], [a])
split l n
  | length l <= n = (l, [])
  | otherwise = (take n l, drop n l)

-- Extract a slice from a list.
-- λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
--    "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice list start end
  | start < 1 || end > length list || start > end = error "Bad indexes."
  | otherwise = take (end - start + 1) (drop (start - 1) list)

-- Rotate a list N places to the left.
-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
--    "defghabc"
--λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
--   "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate list n
  | n == 0 = []
  | n > 0 = drop n list ++ take n list
  | otherwise = reverse $ rotate (reverse list) (-n)

-- Remove the K'th element from a list.
-- λ> removeAt 2 "abcd"
--    ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "List is empty"
removeAt n list
  | n < 1 || n > length list = error "Bad index."
  | otherwise = (list !! (n - 1), take (n - 1) list ++ drop n list)
