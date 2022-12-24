{-# LANGUAGE OverloadedStrings #-}

module Days.Day10 (runDay) where

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
import Data.Functor
import Control.Applicative ((<|>))
import Util.Util
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseInstruction `sepBy` "\n"
  where
    parseInstruction = parseNoop <|> parseAddX
    parseNoop = string "noop" $> Noop
    parseAddX = string "addx " >> AddX <$> signed decimal

------------ TYPES ------------
type Input = [CpuInstruction]

data CpuInstruction = Noop | AddX Int deriving (Show, Eq)

data CpuState = CpuState
  { -- The instruction that is being executed during the current cycle
    pendingInstruction :: CpuInstruction,
    -- The number of cycles the current instruction has been executing
    -- 0 if it the current cycle is the first cycle it is being executed
    -- 1 if it also has been executing during the previous cycle
    pendingCycles :: Int,
    -- The current cycle being executed. The first cycle is numbered 1.
    currentCycle :: Int,
    -- The value of the register X during the current cycle, i.e. it's value after having completed the previous cycle
    x :: Int,
    remainingInstructions :: [CpuInstruction],
    screen :: String
  }
  deriving (Show)

type OutputA = Int

type OutputB = DisplayString

------------ PART A ------------
{- ORMOLU_DISABLE -}
partA :: Input -> OutputA
partA input = error "haha"
{- ORMOLU_ENABLE -}

runCpu :: Int -> CpuState -> CpuState
runCpu cycleCount cpuState =
  if cpuState.currentCycle >= cycleCount
    then cpuState
    else -- finish the pending instruction and start next instruction
      runCpu cycleCount (advanceCpu cpuState)

advanceCpu :: CpuState -> CpuState
advanceCpu cpuState =
  case cpuState.pendingInstruction of
    Noop ->
      CpuState
        { pendingInstruction = head cpuState.remainingInstructions,
          pendingCycles = 0,
          currentCycle = cpuState.currentCycle + 1,
          x = cpuState.x,
          remainingInstructions = tail cpuState.remainingInstructions,
          screen = executeCRT cpuState
        }
    AddX n ->
      -- The AddX instruction takes 2 cycles to complete
      case cpuState.pendingCycles of
        0 ->
          cpuState
            { pendingCycles = 1,
              currentCycle = cpuState.currentCycle + 1,
              screen = executeCRT cpuState
            }
        1 ->
          CpuState
            { pendingInstruction = head cpuState.remainingInstructions,
              pendingCycles = 0,
              currentCycle = cpuState.currentCycle + 1,
              x = cpuState.x + n,
              remainingInstructions = tail cpuState.remainingInstructions,
              screen = executeCRT cpuState
            }

debugOutput :: CpuState -> String
debugOutput cpuState =
  "Current cycle:"
    ++ show cpuState.currentCycle
    ++ "\n"
    ++ "Pending instruction:"
    ++ show cpuState.pendingInstruction
    ++ "\n"
    ++ "X:"
    ++ show cpuState.x
    ++ "\n"
    ++ "Drawing pixel:"
    ++ show (currentPixel cpuState)
    ++ "\n\n"

-- The CRT draws DURING a cycle
currentPixel cpuState =
  if (crtPosition == cpuState.x - 1)
    || crtPosition == cpuState.x
    || (crtPosition == cpuState.x + 1)
    then '#'
    else '.'
  where
    crtPosition = (cpuState.currentCycle - 1) `mod` 40

executeCRT cpuState = --debugOutput cpuState ++ cpuState.screen

  let result = currentPixel cpuState : cpuState.screen
   in if isEndOfLine then '\n' : result else result
  where
    isEndOfLine = cpuState.currentCycle `mod` 40 == 0

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let finalState =
        runCpu
          240
          CpuState
            { pendingInstruction = head input,
              pendingCycles = 0,
              currentCycle = 1,
              x = 1,
              remainingInstructions = tail input,
              screen = ""
            }
{- ORMOLU_DISABLE -}
  --  in DisplayString $ finalState.screen
   in DisplayString $ reverse finalState.screen
{- ORMOLU_ENABLE -}
