{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-asserts #-}

{-# HLINT ignore "Use <$>" #-}
module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Prelude hiding (Left, Right)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Control.Exception
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseInstruction `sepBy` "\n"

parseInstruction = do
  direction <- parseDirection
  char ' '
  amount <- decimal
  return $ Instruction direction amount

parseDirection =
  char 'R' $> Right
    <|> char 'D' $> Down
    <|> char 'U' $> Up
    <|> char 'L' $> Left

------------ TYPES ------------
type Input = [Instruction]

data Instruction = Instruction Direction Int deriving (Show)

data Direction = Up | Down | Left | Right deriving (Show)

type Coord = (Int, Int)

data State = State {knots :: [Coord], tailVisits :: [Coord]} deriving (Show)

knotCount = 10

initialState = State (replicate knotCount (0, 0)) [(0, 0)]

-- type OutputA = [Coord]

type OutputA = Int

type OutputB = OutputA

------------ PART A ------------
{- ORMOLU_DISABLE -}
partA :: Input -> OutputA
partA input =
  let state = foldl' (flip applyInstruction) initialState input
  --  in state.tailVisits
   in length $ Set.fromList state.tailVisits
{- ORMOLU_ENABLE -}

moveUp :: Coord -> Coord
moveUp (row, col) = (row - 1, col)

moveDown :: Coord -> Coord
moveDown (row, col) = (row + 1, col)

moveLeft :: Coord -> Coord
moveLeft (row, col) = (row, col - 1)

moveRight :: Coord -> Coord
moveRight (row, col) = (row, col + 1)

mapDirectionToSuccessor direction = case direction of
  Up -> moveUp
  Down -> moveDown
  Left -> moveLeft
  Right -> moveRight

moveUpLeft :: Coord -> Coord
moveUpLeft (row, col) = (row - 1, col - 1)

moveUpRight :: Coord -> Coord
moveUpRight (row, col) = (row - 1, col + 1)

moveDownLeft :: Coord -> Coord
moveDownLeft (row, col) = (row + 1, col - 1)

moveDownRight :: Coord -> Coord
moveDownRight (row, col) = (row + 1, col + 1)

straightSuccessors = [moveUp, moveLeft, moveDown, moveRight]

diagonalSuccessors = [moveUpLeft, moveUpRight, moveDownLeft, moveDownRight]

areAdjacent :: Coord -> Coord -> Bool
areAdjacent (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

-- Whenever head is not adjacent to tail:
-- - If head and tail are in same row or column, the tail moves in the same direction as the head.
--   This is unique, so you can also just try all 4 directions and test for adjacency
-- - Otherwise the tail moves diagonally in a direction such that it is adjacent again.
--   This is unique, so you can just try all 4 directions and test for adjacency
moveHead direction state =
  let successor = mapDirectionToSuccessor direction
      (currentHead : tails) = state.knots
      newHead = successor currentHead
      newKnots = newHead : computeNewKnots newHead tails
   in State {knots = newKnots, tailVisits = newKnots !! 9 : state.tailVisits}

computeNewKnots :: Coord -> [Coord] -> [Coord]
computeNewKnots newHead (currentTail : remainingTails) = newTail : computeNewKnots newTail remainingTails
  where
    newTail = computeNewTail newHead currentTail
computeNewKnots _ [] = []

computeNewTail :: Coord -> Coord -> Coord
computeNewTail newHead currentTail
  -- Do not move if still adjacent
  | areAdjacent newHead currentTail = currentTail
  -- Prefer moving straight if the head is in the same row or column
  | fst currentTail == fst newHead || snd currentTail == snd newHead = firstAdjacentBySuccessors straightSuccessors
  -- Prefer moving diagonally otherwise
  | otherwise = firstAdjacentBySuccessors (diagonalSuccessors ++ straightSuccessors)
  where
    firstAdjacentBySuccessors successors =
      let newTailCandidates = map (\succ -> succ currentTail) successors
       in Prelude.head $ filter (areAdjacent newHead) newTailCandidates

applyInstruction :: Instruction -> State -> State
applyInstruction (Instruction direction amount) state =
  if amount <= 0
    then state
    else applyInstruction (Instruction direction (amount - 1)) (moveHead direction state)

------------ PART B ------------
partB :: Input -> OutputB
partB = partA
