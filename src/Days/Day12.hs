{-# LANGUAGE BangPatterns #-}

module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Array
import Data.Functor
import Data.Char (ord)
import Control.Applicative ((<|>))
import Util.Util
import Util.AStar
import Control.Monad (when)
import GHC.Char (chr)
import Debug.Trace (trace, traceShow)
import Data.Monoid (Sum(..))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseNestedList

listToGrid :: Input -> Grid
listToGrid list = listArray ((0, 0), (height - 1, width - 1)) $ concat list
  where
    height = length list
    width = length (head list)

gridDimensions :: Grid -> (Int, Int)
gridDimensions grid = let (_, (row, col)) = bounds grid in (row + 1, col + 1)

parseNestedList :: Parser [[Height]]
parseNestedList = do
  many1' point `sepBy` "\n"
  where
    startPoint = char 'S' $> 1
    endPoint = char 'E' $> 27
    intermediatePoint = do
      c <- satisfy $ inClass "a-z"
      return $ ord c - 96
    point = startPoint <|> endPoint <|> intermediatePoint

------------ TYPES ------------
type Coord = (Int, Int) -- (row, column)

type Height = Int

type Input = [[Height]]

type Grid = Array Coord Height

type OutputA = DisplayString

type OutputB = [Sum Double]

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  let grid = listToGrid input
      (height, width) = spyMsg "dimensions" $ gridDimensions grid
      sortedCoords = fst <$> sortBy (\(_, a) (_, b) -> compare a b) (assocs grid)
      !start = spyMsg "Start" $ head sortedCoords
      !end = spyMsg "End" $ last sortedCoords
      coordValidator (row, col) = (0 <= col && col < width && 0 <= row && row < height)
      (_, costMap) = astar (successor grid coordValidator) (manhattanDistance end) (== end) start
      shader coord = chr (maybe 45 (\x -> (round x.getSum `mod` 57) + 65) (costMap HashMap.!? coord))
   in traceShow
        (displayGrid $ drawGrid shader width height)
        ( case costMap HashMap.!? end of
            Nothing -> DisplayString "End was not found in the cost map"
            Just endCost -> DisplayString $ show endCost
        )

manhattanDistance :: (Int, Int) -> (Int, Int) -> Sum Double
manhattanDistance (a, b) (c, d) = abs (fromIntegral a - fromIntegral c) + abs (fromIntegral b - fromIntegral d)

successor :: Grid -> (Coord -> Bool) -> (Coord, Sum Double) -> [(Coord, Sum Double)]
successor heightMap isValid (n@(row, col), parentCost) =
  let left = (row, col - 1)
      up = (row - 1, col)
      down = (row + 1, col)
      right = (row, col + 1)
      currentHeight = heightMap ! n
      isReachable coord = ((heightMap ! coord) - currentHeight) <= 1
      withCosts c = (c, parentCost + 1) :: (Coord, Sum Double)
   in withCosts <$> filter isReachable (filter isValid [left, up, down, right])

drawGrid :: (Coord -> Char) -> Int -> Int -> [String]
drawGrid shader width height = [[shader (row, col) | col <- [0 .. width - 1]] | row <- [0 .. height - 1]]

displayGrid :: [String] -> DisplayString
displayGrid lines = DisplayString $ intercalate "\n" lines

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let grid = listToGrid input
      (height, width) = spyMsg "dimensions" $ gridDimensions grid
      end = head $ fst <$> filter (\(_, h) -> h == 27) (assocs grid)
      startingPoints = fst <$> filter (\(_, h) -> h == 1) (assocs grid)
      coordValidator (row, col) = (0 <= col && col < width && 0 <= row && row < height)
      costMap start = snd $ astar (successor grid coordValidator) (manhattanDistance end) (== end) start
   in sort $ (\start -> costMap start HashMap.!? end) `mapMaybe` startingPoints
