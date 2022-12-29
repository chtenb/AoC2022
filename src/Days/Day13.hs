module Days.Day13 (runDay) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, decimal, sepBy, string)
import Data.List (elemIndex, intercalate, sort)
import Data.Maybe (fromJust)
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parsePacketPair `sepBy` "\n"
  where
    parsePacketPair = do
      left <- parseListOrInt <* string "\n"
      right <- parseListOrInt <* string "\n"
      return (left, right)
    parseListOrInt = parseInt <|> parseList
    parseInt = Int <$> decimal
    parseList = do
      string "["
      values <- sepBy parseListOrInt ","
      string "]"
      return $ Lst values

------------ TYPES ------------
type Input = [(ListOrInt, ListOrInt)]

data ListOrInt = Lst [ListOrInt] | Int Int

instance Show ListOrInt where
  show (Int i) = show i
  show (Lst l) = "[" ++ intercalate "," (show <$> l) ++ "]"

instance Ord ListOrInt where
  compare (Int i) (Int j) = compare i j
  compare (Int i) (Lst l) = compare (Lst [Int i]) (Lst l)
  compare (Lst l) (Int i) = compare (Lst l) (Lst [Int i])
  compare (Lst (x : xs)) (Lst (y : ys)) = if x /= y then compare x y else compare (Lst xs) (Lst ys)
  compare (Lst []) (Lst (y : ys)) = LT
  compare (Lst (x : xs)) (Lst []) = GT
  compare (Lst []) (Lst []) = EQ

instance Eq ListOrInt where
  x == y = compare x y == EQ

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = sum $ fst <$> filter (hasExpectedOrder . snd) (zip [1 ..] input)

hasExpectedOrder (left, right) = left < right

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let inputPackets = uncurry (++) $ unzip input
      divider1 = Lst [Lst [Int 2]]
      divider2 = Lst [Lst [Int 6]]
      allPackets = divider1 : divider2 : inputPackets
      sortedPackets = sort allPackets
      index1 = fromJust $ elemIndex divider1 sortedPackets
      index2 = fromJust $ elemIndex divider2 sortedPackets
   in (index1 + 1) * (index2 + 1)
