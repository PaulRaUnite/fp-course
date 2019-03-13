module Main where

import           Pack1

assert :: String -> Bool -> String
assert m cond
  | cond = "True:  " ++ m
  | otherwise = "False: " ++ m

main :: IO ()
main = do
  putStrLn "---------------------------- TESTS ----------------------------"
  putStrLn $ assert "myLast [1,2,3] == 3" (myLast [1, 2, 3] == 3)
  putStrLn $ assert "myLast [1,2,3] != 5" (myLast [1, 2, 3] /= 5)
  putStrLn $ assert "myButLast [1,2,3] == 2" (myButLast [1, 2, 3] == 2)
  putStrLn $ assert "myButLast [1,2,3] != 5" (myButLast [1, 2, 3] /= 5)
  putStrLn $ assert "elementAt [1...,5] 4 == 4" (elementAt [1 .. 5] 4 == 4)
  putStrLn $ assert "myLength [1...,5] == 5" (myLength [1 .. 5] == 5)
  putStrLn $ assert "myLength' [1...,5] == 5" (myLength' [1 .. 5] == 5)
  putStrLn $ assert "myReverse [1...,5] == [5...,1]" (myReverse [1 .. 5] == reverse [1 .. 5])
  putStrLn $ assert "myReverse' [1...,5] == [5...,1]" (myReverse' [1 .. 5] == reverse [1 .. 5])
  putStrLn $ assert "isPalindrome [1,2,3,2,1]" (isPalindrome [1, 2, 3, 2, 1])
  putStrLn $ assert "not isPalindrome [1,1,3,2,1]" (not $ isPalindrome [1, 1, 3, 2, 1])
  putStrLn $ assert "isPalindrome' [1,2,3,2,1]" (isPalindrome' [1, 2, 3, 2, 1])
  putStrLn $ assert "not isPalindrome' [1,2,3,3,1]" (not $ isPalindrome' [1, 2, 3, 3, 1])
  putStrLn $ assert "flatten (Elem 5) == [5]" (flatten (Elem 5) == [5])
  putStrLn $
    assert
      "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])"
      (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1 .. 5])
  putStrLn $ assert "flatten (List []) == []" (null $ flatten (List []))
  putStrLn $ assert "compress \"aaaabccaadeeee\" == \"abcade\"" (compress "aaaabccaadeeee" == "abcade")
  putStrLn $
    assert
      "pack \"aaaabccaadeeee\" == [\"aaaa\", \"b\", \"cc\", \"aa\", \"d\", \"eeee\"]"
      (pack "aaaabccaadeeee" == ["aaaa", "b", "cc", "aa", "d", "eeee"])
  putStrLn $
    assert
      "encode \"aaaabccaadeeee\" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]"
      (encode "aaaabccaadeeee" == [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')])
