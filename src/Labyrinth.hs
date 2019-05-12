{-
module Labyrinth where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.Random (Random, getStdGen, randomR, randomRs)

data Door
  = North
  | East
  | South
  | West
  deriving (Show, Enum, Eq, Ord)

data Maze =
  Maze
    { size :: Int
    , grid :: Map (Int, Int) (Set Door)
    }

instance Show Maze where
  show m =
    concatMap
      (\i ->
         let (f, s, t) = unzip3 (map (\j -> renderCell (grid m Map.! (i, j))) [1 .. (size m)])
          in (concat f ++ "\n" ++ concat s ++ "\n" ++ concat t ++ "\n"))
      [1 .. (size m)]
    where
      renderCell ds =
        ( "*" ++ renderDoor ds North ++ "*"
        , renderDoor ds West ++ " " ++ renderDoor ds East
        , "*" ++ renderDoor ds South ++ "*")
      renderDoor ds d
        | Set.member d ds = " "
        | otherwise = "*"

generateMaze :: Int -> Float -> Float -> IO Maze
generateMaze n c d = do
    gen <- getStdGen
    return $ Maze n foldl (\m _-> let (x, gen) = randomR (1,n-1) gen
                                      (y, gen) = randomR (1,n-1) gen in ) Map.empty [1..density]
  where
    density = round (fromIntegral n * d)
    complexity = round (10 * (fromIntegral n * c))
-}
module Labyrinth where

import           Data.Array
import           Data.List           (delete, find)

import           Control.Monad.State
import           Prelude

import           Data.Maybe
import           Debug.Trace         (trace)

{- --------------------------------------------------------------------------
 - Stolen maze generator.
 -
 -
 - Author: Jamis Buck (jamis@jamisbuck.org)
 - ------------------------------------------------------------------------ -}
import           System.Random

-- tiny kernel for our maze generation problem;
-- all it does is carry around our random-number generator
type MazeM a = State StdGen a -- our MazeM monad is just a State monad

rnd :: (Int, Int) -> MazeM Int
rnd rng -- kernel function to generate a random num
 = do
  (x, gen') <- gets (randomR rng)
  put gen'
  return x

data Dir
  = North
  | West
  | East
  | South
  deriving (Eq, Enum, Show)

data PointA a =
  Pt
    { x, y :: a
    }
  deriving (Eq, Ord, Ix, Show)

type Point = PointA Int

type Maze = Array Point [Dir]

instance Num a => Num (PointA a) where
  (Pt a b) + (Pt c d) = Pt (a + c) (b + d)
  (Pt a b) - (Pt c d) = Pt (a - c) (b - d)
  (Pt a b) * (Pt c d) = Pt (a * c) (b * d)
  abs (Pt a b) = Pt (abs a) (abs b)
  signum (Pt a b) = Pt (signum a) (signum b)
  fromInteger i = Pt (fromInteger i) (fromInteger i)

-- Get the next random Int less than ceil from the given generator
nexti :: Int -> MazeM Int
nexti ceil = rnd (0, ceil - 1)

-- Return the width of the given maze
width :: Maze -> Int
width maze = x $ snd (bounds maze)

-- Return the height of the given maze
height :: Maze -> Int
height maze = y $ snd (bounds maze)

-- Find and return a random point in the maze that has already been visited
fixPos :: Maze -> MazeM Point
fixPos maze = do
  x <- nexti (width maze)
  y <- nexti (height maze)
  let pt = Pt x y
  if null (maze ! pt)
    then fixPos maze
    else return pt

-- Check to see whether moving in the given direction from the given point
-- does not take us beyond the bounds of the maze
moveIsInBounds :: Maze -> Point -> Dir -> Bool
moveIsInBounds maze pos North = y pos > 0
moveIsInBounds maze pos West  = x pos > 0
moveIsInBounds maze pos East  = x pos + 1 < width maze
moveIsInBounds maze pos South = y pos + 1 < height maze

-- Check to see whether the move from the given point in the given direction
-- is legal (meaning, it won't take us beyond the bounds of the array, and
-- the cell in that direction is unvisited)
availableDirection :: Maze -> Point -> Dir -> Bool
availableDirection maze pos dir = moveIsInBounds maze pos dir && null (maze ! movePos pos dir)

-- Update the maze such that a connection is created between the given cell
-- and the neighboring cell in the given direction
assertConnection :: Maze -> Point -> Dir -> Maze
assertConnection maze pos dir = maze // changes
  where
    newPos = movePos pos dir
    changes = [(pos, dir : (maze ! pos)), (newPos, oppositeDir dir : (maze ! newPos))]

-- Return True if the given list of directions includes the given direction
hasDir :: Dir -> [Dir] -> Bool
hasDir dir dirs = Data.Maybe.isJust (find (== dir) dirs)

-- Return a new point that is the result of moving from the given point in the
-- given direction
movePos :: Point -> Dir -> Point
movePos (Pt x y) North = Pt x (y - 1)
movePos (Pt x y) West  = Pt (x - 1) y
movePos (Pt x y) East  = Pt (x + 1) y
movePos (Pt x y) South = Pt x (y + 1)

-- Given a direction, return the opposite direction
oppositeDir :: Dir -> Dir
oppositeDir North = South
oppositeDir West  = East
oppositeDir East  = West
oppositeDir South = North

-- Generate a maze using the given random generator. 'count' is the number of
-- cells that are still unvisited. 'maze' is the current state of the maze,
-- 'pos' is the current position in the maze, and 'dirs' is an array of
-- directions that have not been tried from the current position.
mazeGen :: Int -> Maze -> Point -> [Dir] -> MazeM Maze
-- if there are no more cells that need to be visited, we're done
mazeGen 0 maze _ _ = return maze
-- if there are no more directions that we can try, we need to choose a new
-- (visited) point and start fresh from there
mazeGen count maze _ [] = do
  newPos <- fixPos maze
  mazeGen count maze newPos [North .. South]
-- otherwise, choose a new direction from the list of untried directions.
-- If we can, move in that direction, otherwise recurse and try another
-- direction.
mazeGen count maze pos dirs = do
  dirIdx <- nexti (length dirs)
  let dir = dirs !! dirIdx
      valid = availableDirection maze pos dir
  if valid
    -- move in the given direction
    then mazeGen (count - 1) (assertConnection maze pos dir) (movePos pos dir) [North .. South]
    -- can't move that way, so we try again
    else mazeGen count maze pos (delete dir dirs)

-- generate a new maze (dimensions 'w' x 'h') using the given random generator.
maze :: StdGen -> Int -> Int -> Maze
maze gen w h
    -- this is the key; this is where the State gets created
 = evalState (mazeGen (w * h - 1) maze0 (Pt 0 0) [North .. South]) gen
  where
    maze0 = array (Pt 0 0, Pt w h) [(Pt i j, []) | i <- [0 .. w], j <- [0 .. h]]

-- return a string containing an ASCII rendering of the maze
render :: Maze -> String
render maze = renders maze 0

-- return a string containing an ASII rendering of the maze, starting at
-- the given row
renders :: Maze -> Int -> String
renders maze row =
  if row < height maze
    then renderRow maze row ++ "\n" ++ renderUnderRow maze row ++ "\n" ++ renders maze (row + 1)
    else ""
  where
    renderRow maze row = concat [renderCell (maze ! Pt x row) | x <- [0 .. width maze - 1]]
    renderCell dirs = renderh West dirs ++ "+" ++ renderh East dirs
    renderh dir dirs =
      if hasDir dir dirs
        then "-"
        else " "
    renderUnderRow maze row = concat [renderUnderCell $ maze ! Pt x row | x <- [0 .. width maze - 1]]
    renderUnderCell dirs =
      " " ++
      (if hasDir South dirs
         then "|"
         else " ") ++
      " "

-- MY solver (left hand based)
nextDir :: Dir -> Dir
nextDir North = East
nextDir East  = South
nextDir South = West
nextDir West  = North

returnDir :: Point -> Point -> Dir
returnDir from to = dir
  where
    dir =
      case to - from of
        (Pt (-1) _) -> West
        (Pt 1 _)    -> East
        (Pt _ 1)    -> South
        (Pt _ (-1)) -> North

chooseNext :: Maze -> Point -> [Point] -> Point
chooseNext maze pos [] =
  let dir = head (maze ! pos)
   in movePos pos dir
chooseNext maze pos path = chooseNext0 maze pos $ nextDir $ returnDir pos (head path)
  where
    chooseNext0 maze pos dir =
      if dir `elem` (maze ! pos)
        then movePos pos dir
        else chooseNext0 maze pos (nextDir dir)

correctPath :: Point -> [Point] -> [Point]
correctPath pos path = path

solveMaze :: Maze -> Point -> Point -> [Point]
solveMaze maze start end = solveMaze0 maze start end []
  where
    solveMaze0 maze current end path
      | current == end = path
      | otherwise =
        let nextP = chooseNext maze current path
         in solveMaze0 maze nextP end $ correctPath nextP (current : path)
