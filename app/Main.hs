module Main where

import           Data.Array
import           Labyrinth
import           Pack1
import           System.Random (getStdGen, randomRs)

main :: IO ()
main = do
  let w = 10
  let h = 10
  gen <- getStdGen
  let m = maze gen w h
  putStrLn $ render m
  let pathM = array (Pt 0 0, Pt w h) [(Pt i j, []) | i <- [0 .. w - 1], j <- [0 .. h - 1]]
  let (h:t) = solveMaze m (Pt 0 0) (Pt 9 9)
  let (_, changes) =
        foldl
          (\(y, acc) x ->
             ( x
             , [ ( y
                 , (if not $ null acc
                      then let (_, dirs) = head acc
                            in dirs
                      else []) ++
                   [returnDir y x])
               , (x, [returnDir x y])
               ] ++
               acc))
          (h, [])
          t
  putStrLn $ render $ pathM // changes
