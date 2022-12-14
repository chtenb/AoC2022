module Days.Day05 (runDay) where

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
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (foldM)
import GHC.Arr
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseInstruction `sepBy` "\n"

parseInstruction = do
  _ <- string "move "
  amount <- decimal
  _ <- string " from "
  from <- decimal
  _ <- string " to "
  to <- decimal
  return Instruction {amount, from, to}

------------ TYPES ------------
type Input = [Instruction]

data Instruction = Instruction {amount :: Int, from :: Int, to :: Int} deriving (Show)

type OutputA = String

type OutputB = String

-- initialStacks :: forall s. ST s (STArray s Int [Char])
-- initialStacks = do
--   newListArray
--     (1, 3)
--     [ reverse ['Z', 'N'],
--       reverse ['M', 'C', 'D'],
--       reverse ['P']
--     ]

initialStacks :: forall s. ST s (STArray s Int [Char])
initialStacks = do
  newListArray
    (1, 9)
    [ reverse ['S', 'Z', 'P', 'D', 'L', 'B', 'F', 'C'],
      reverse ['N', 'V', 'G', 'P', 'H', 'W', 'B'],
      reverse ['F', 'W', 'B', 'J', 'G'],
      reverse ['G', 'J', 'N', 'F', 'L', 'W', 'C', 'S'],
      reverse ['W', 'J', 'L', 'T', 'P', 'M', 'S', 'H'],
      reverse ['B', 'C', 'W', 'G', 'F', 'S'],
      reverse ['H', 'T', 'P', 'M', 'Q', 'B', 'W'],
      reverse ['F', 'S', 'W', 'T'],
      reverse ['N', 'C', 'R']
    ]

------------ PART A ------------
partA :: Input -> OutputA
partA instructions = runST $ do
  stacks <- initialStacks
  finalStacks <- foldM executeInstruction stacks instructions >>= freezeSTArray
  return $ foldrElems (\a b -> if null a then b else head a : b) "" finalStacks

-- return $ show finalStacks

executeInstruction :: forall s. STArray s Int [Char] -> Instruction -> ST s (STArray s Int [Char])
executeInstruction stacks (Instruction {amount, from, to}) = do
  fromStack <- readArray stacks from
  case amount of
    0 -> return stacks
    x -> case fromStack of
      movedElement : newFrom -> do
        toStack <- readArray stacks to
        let newTo = movedElement : toStack
        writeArray stacks from newFrom
        writeArray stacks to newTo
        executeInstruction stacks (Instruction {amount = amount - 1, from, to})
      [] -> return stacks

------------ PART B ------------
partB :: Input -> OutputB
partB instructions = runST $ do
  stacks <- initialStacks
  finalStacks <- foldM executeInstruction' stacks instructions >>= freezeSTArray
  return $ foldrElems (\a b -> if null a then b else head a : b) "" finalStacks

executeInstruction' :: forall s. STArray s Int [Char] -> Instruction -> ST s (STArray s Int [Char])
executeInstruction' stacks (Instruction {amount, from, to}) = do
  fromStack <- readArray stacks from
  let movedElements = take amount fromStack
  let newFrom = drop amount fromStack
  toStack <- readArray stacks to
  let newTo = movedElements ++ toStack
  writeArray stacks from newFrom
  writeArray stacks to newTo
  return stacks
