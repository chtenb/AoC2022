module Days.Day02 (runDay) where

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
import Data.Attoparsec.Text ( Parser, sepBy, char )
import Data.Void
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseRound2 `sepBy` "\n"

parseRound2 = do
  opponent <- parseOpponent
  char ' '
  outcome <- parseOutcome
  return $ Round {opponent, outcome}

parseOpponent :: Parser Play
parseOpponent =
  (Rock <$ char 'A')
    <|> (Paper <$ char 'B')
    <|> (Scissors <$ char 'C')

parseOutcome :: Parser Outcome
parseOutcome =
  (Lose <$ char 'X')
    <|> (Draw <$ char 'Y')
    <|> (Win <$ char 'Z')

------------ TYPES ------------
data Play = Rock | Paper | Scissors deriving (Show, Eq)

data Outcome = Win | Lose | Draw deriving (Show, Eq)

data Round = Round {opponent :: Play, outcome :: Outcome} deriving (Show)

type Input = [Round]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = error "" -- sum . map scoreInput

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map scoreRound

winner opponent = case opponent of
  Rock -> Paper
  Paper -> Scissors
  Scissors -> Rock

loser opponent = case opponent of
  Paper -> Rock
  Scissors -> Paper
  Rock -> Scissors

whatShouldIPlay opponent outcome =
  if outcome == Draw then opponent else if outcome == Win then winner opponent else loser opponent

scorePlay play = case play of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

scoreRound (Round {opponent, outcome}) =
  scoreOutcome outcome + (scorePlay $ whatShouldIPlay opponent outcome)

scoreOutcome :: Outcome -> Int
scoreOutcome outcome = case outcome of
  Win -> 6
  Draw -> 3
  Lose -> 0
