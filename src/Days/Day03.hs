module Days.Day03 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Data.Char
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1' (satisfy ('\n' /=)) `sepBy` "\n"

------------ TYPES ------------
type Input = [[Char]]

type OutputA = Int

type OutputB = Int

type RuckSack = (Set Char, Set Char)

type Group = [Set Char]

------------ PART A ------------
partA :: Input -> OutputA
partA = error ""

inputToRuckSack :: [Char] -> RuckSack
inputToRuckSack chars = (Set.fromList $ take half chars, Set.fromList $ drop half chars)
  where
    half = length chars `div` 2

findCommonItem (left, right) = head $ Set.toList $ Set.intersection left right

itemPriority char = 1 + if isAsciiLower char then ord char - ord 'a' else 26 + ord char - ord 'A'

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (itemPriority . findCommonGroupItem) . toGroups

toGroups :: Input -> [Group]
toGroups = U.chunksOf 3 . map Set.fromList

findCommonGroupItem [a, b, c] = head $ Set.toList $ Set.intersection a (Set.intersection b c)
