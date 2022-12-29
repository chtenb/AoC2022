{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Days.Day14 (runDay) where

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
import Data.Array.ST
import Control.Monad.ST
import Data.Bifunctor (bimap)
import Control.Monad (foldM, foldM_)
import Util.Util
import Data.Array
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseRockPath `sepBy` "\n"
  where
    parseRockPath = parseCoord `sepBy` " -> "
    parseCoord = do
      x <- decimal
      string ","
      y <- decimal
      return $ Coord x y

------------ TYPES ------------
data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)

toTuple c = (c.x, c.y)

fromTuple (x, y) = Coord x y

instance Ord Coord where
  compare a b = compare (toTuple a) (toTuple b)

instance Ix Coord where
  range coords = fromTuple <$> range (bimap toTuple toTuple coords)
  inRange coords c = inRange (bimap toTuple toTuple coords) (toTuple c)
  index coords c = index (bimap toTuple toTuple coords) (toTuple c)

type RockPath = [Coord]

type Input = [RockPath]

type OutputA = DisplayString

type OutputB = Void

type Grid s = (STUArray s Coord Char)

initialGrid :: forall s. Input -> ST s (Grid s)
initialGrid input = do
  let minX = (-1) + minimum (x <$> concat input)
  let maxX = 1 + maximum (x <$> concat input)
  let minY = 0 -- (-1) + minimum (y <$> concat input)
  let maxY = 1 + maximum (y <$> concat input)
  newListArray
    (Coord minX minY, Coord maxX maxY)
    (repeat '.')

expandRockPath :: RockPath -> [Coord]
expandRockPath [] = []
expandRockPath [c] = [c]
expandRockPath (from : to : cs) = expandRockLine ++ expandRockPath (to : cs)
  where
    range a b = [a, if a < b then a + 1 else a - 1 .. b]
    expandRockLine
      -- vertical
      | from.x == to.x = (\y -> Coord from.x y) <$> range from.y to.y
      -- horizontal
      | from.y == to.y = (\x -> Coord x from.y) <$> range from.x to.x
      | otherwise = error "not a straight line"

drawRocks :: forall s. Grid s -> [Coord] -> ST s ()
drawRocks grid coords = do
  let writeCoord _ coord = writeArray grid coord '#'
  foldM_ writeCoord () coords

drawRockPath :: forall s. Grid s -> RockPath -> ST s ()
drawRockPath grid path = drawRocks grid (expandRockPath path)

drawRockPaths :: forall s. Grid s -> [RockPath] -> ST s ()
drawRockPaths grid = foldM_ (const $ drawRockPath grid) ()

drawGrid :: Array Coord Char -> [String]
drawGrid grid =
  let (minC, maxC) = bounds grid
   in [[grid ! Coord x y | x <- [minC.x .. maxC.x]] | y <- [minC.y .. maxC.y]]

displayGrid :: [String] -> DisplayString
displayGrid lines = DisplayString $ intercalate "\n" lines

------------ PART A ------------
partA :: Input -> OutputA
partA input = runST $ do
  grid <- initialGrid input
  drawRockPaths grid input
  result <- freeze grid
  return $ displayGrid (drawGrid result)

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
