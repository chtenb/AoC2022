module Days.Day04 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseRangePair `sepBy` "\n"

parseRangePair = do
  a <- decimal
  _ <- char '-'
  b <- decimal
  _ <- char ','
  c <- decimal
  _ <- char '-'
  d <- decimal
  return ((a, b), (c, d))

------------ TYPES ------------
type Input = [RangePair]

type Range = (Int, Int)

type RangePair = (Range, Range)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter (uncurry reportRangePair)

reportRangePair r1 r2 = rangeContainsRange r1 r2 || rangeContainsRange r2 r1

rangeContainsRange (a, b) (c, d) = a <= c && b >= d

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter (uncurry rangesOverlap)

rangesOverlap (a, b) (c, d) = (a <= c && c <= b) || (c <= a && a <= d)
