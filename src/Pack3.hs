module Pack3 where

import           Control.Monad (replicateM)
import           Data.List     (sortBy, tails)
import           Data.Map      (empty, insertWith, (!))
import           Pack1         (elementAt)
import           System.Random (Random, getStdGen, randomR, randomRs)

-- Insert an element at a given position into a list.
-- λ> insertAt 'X' "abcd" 2
--    "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt v list pos
  | pos <= 0 || actual_pos > length list = error "Wrong index."
  | otherwise = take actual_pos list ++ [v] ++ drop actual_pos list
  where
    actual_pos = pos - 1

-- Create a list containing all integers within a given range.
-- λ> range 4 9
--    [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range from to = [from .. to]

betterRange :: (Enum a, Ord a) => a -> a -> [a]
betterRange from to
  | from > to = error "Wrong bounds."
  | from == to = [from]
  | otherwise = from : betterRange (succ from) to

-- Extract a given number of randomly selected elements from a list.
-- λ> rndSelect "abcdefgh" 3 >>= putStrLn
--    eda
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
  gen <- getStdGen
  return $ take n [elementAt xs (x + 1) | x <- randomRs (0, length xs - 1) gen]

-- Return N different random numbers from the set 1..M.
-- λ> diffSelect 6 49
--    [23,1,17,33,21,37]
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m
  | m < 1 = error "Wrong upper bound."
  | otherwise = do
    gen <- getStdGen
    return . take n $ randomRs (1, m) gen

permutation :: [a] -> Int -> Int -> [a]
permutation list f s
  | f < 0 || f > length list - 1 || s < 0 || s > length list - 1 = error "Wrong indexes"
  | f == s = list
  | s < f = permutation list s f
  | otherwise = fl ++ [list !! s] ++ fsl ++ [list !! f] ++ sl
  where
    fl = take f list
    fsl = take (s - f - 1) $ drop (f + 1) list
    sl = drop (s + 1) list

-- Generate a random permutation of the elements of a list.
-- λ> rndPermutation "abcdef"
--    "badcef"
rndPermutation :: [a] -> IO [a]
rndPermutation list = do
  gen1 <- getStdGen
  let (first, gen2) = randomR (0, length list - 1) gen1
  let (second, gen3) = randomR (0, length list - 1) gen2
  return $ permutation list first second

-- Generate the combinations of K distinct objects chosen from the N elements of a list.
-- λ> combinations 3 "abcdef"
--    ["abc","abd","abe",...]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations _ [] = []
combinations 1 xs = map (: []) xs
combinations n xs = [y : ys | y:xs' <- tails xs, ys <- combinations (n - 1) xs']

-- Group list members into sublist of 2, 3, and 4
group3 :: Eq a => [a] -> [[[a]]]
group3 = group (2, 3, 4)

toIndexes :: [a] -> Int -> a
toIndexes list x = list !! x

-- Group list members into sublist of specific lengths.
-- λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
--    [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
--    (altogether 1260 solutions)
-- λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
--    [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
--    (altogether 756 solutions)
group :: Eq a => (Int, Int, Int) -> [a] -> [[[a]]]
group (f, s, t) list =
  map
    (map (map (list !!)))
    [ [fl, sl, [x | x <- indexes, x `notElem` (fl ++ sl)]]
    | fl <- combinations f indexes
    , sl <- combinations s [x | x <- indexes, x `notElem` fl]
    ]
  where
    indexes = [0 .. (length list - 1)]

-- Length sorting.
-- λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
--    ["o","de","de","mn","abc","fgh","ijkl"]
lsort :: Ord a => [[a]] -> [[a]]
lsort = sortBy (\x y -> compare (length x) (length y))

-- Length frequency sorting.
-- λ> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
--    ["ijkl","o","abc","fgh","de","de","mn"]
lfsort :: Ord a => [[a]] -> [[a]]
lfsort list = sortBy (\x y -> compare (frequency x) (frequency y)) list
  where
    frequencies = foldr (\x -> insertWith (+) (length x) 1) empty list
    frequency x = frequencies ! length x
