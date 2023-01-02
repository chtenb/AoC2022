{-# OPTIONS_GHC -fno-ignore-asserts #-}

module Days.Day17 (runDay) where

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
import Control.Applicative
import Util.Util
import Debug.Trace
import Control.Exception (assert)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' $ (string "<" $> PushToLeft) <|> (string ">" $> PushToRight)

------------ TYPES ------------
type Input = [Jet]

type Coord = (Int, Int)

-- (0,0) is the lower left corner of the covering rectangle of a shape
type Shape = [Coord]

shape1 = [(0, 0), (1, 0), (2, 0), (3, 0)]

shape2 = [(1, 0), (0, 1), (1, 1), (1, 2), (2, 1)]

shape3 = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]

shape4 = [(0, 0), (0, 1), (0, 2), (0, 3)]

shape5 = [(0, 0), (0, 1), (1, 0), (1, 1)]

data Jet = PushToLeft | PushToRight deriving (Show, Eq)

data FallingRock = FallingRock {position :: Coord, shape :: Shape} deriving (Show)

type Chamber = Set Coord

data ShapeJetIndexCombination = ShapeJetIndexCombination {shapeIndex' :: Int, jetIndex' :: Int} deriving (Eq, Ord)

instance Show ShapeJetIndexCombination where
  show s = "(" ++ show s.shapeIndex' ++ "," ++ show s.jetIndex' ++ ")"

data State = State {chamber :: Chamber, shapeQueue :: [Shape], jetQueue :: [Jet], fallingRock :: FallingRock, towerHeight :: Int, jetCount :: Int, jetsSpawned :: Int, rocksSpawned :: Int, jetIndex :: Int, rockIndex :: Int, indexCombinations :: Map ShapeJetIndexCombination (Int, Int, Int)}

instance Show State where
  show state =
    -- show state.chamber ++ "\n" ++
    show state.fallingRock ++ "\n" ++ show state.towerHeight ++ "\n" ++ show state.jetIndex ++ "/" ++ show state.jetCount ++ " (" ++ show state.jetsSpawned ++ ")\n" ++ show state.rockIndex ++ "/5 (" ++ show state.rocksSpawned ++ ")"

-- rockCount = 2022

rockCount = 1000000000000

-- rockCount = 2022

-- rockCount = 50455
initialState input =
  spy
    State
      { -- The floor is made up of the coordinates (0,0) ... (6,0)
        chamber = Set.fromList $ (,0) <$> [0 .. 6],
        shapeQueue = tail shapeList,
        jetQueue = cycle input,
        fallingRock = FallingRock (2, 4) (head shapeList),
        towerHeight = 0,
        indexCombinations = Map.empty,
        jetsSpawned = 0,
        rocksSpawned = 0,
        jetIndex = 0,
        rockIndex = 0,
        jetCount = length input
      }
  where
    shapeList = take rockCount $ cycle [shape1, shape2, shape3, shape4, shape5]

simulate terminator state =
  let jet = head state.jetQueue
      pushedRock = pushRock state.chamber state.fallingRock jet
      jetsSpawned = (state.jetsSpawned + 1)
      jetIndex = (state.jetIndex + 1) `mod` state.jetCount

      rocksSpawned = (state.rocksSpawned + 1)
      rockIndex = (state.rockIndex + 1) `mod` 5
      (fallenRock, hasFallen) = fallDown state.chamber pushedRock
   in assert (jetsSpawned `mod` state.jetCount == jetIndex) $
        if hasFallen
          then simulate terminator state {jetQueue = tail state.jetQueue, fallingRock = fallenRock, jetsSpawned, jetIndex}
          else
            let newState =
                  state
                    { chamber = Set.union state.chamber $ Set.fromList $ rockCoords fallenRock,
                      jetQueue = tail state.jetQueue,
                      towerHeight = max state.towerHeight (maxYCoord $ rockCoords fallenRock),
                      jetsSpawned,
                      jetIndex,
                      rocksSpawned,
                      rockIndex
                    }
             in if terminator newState
                  then newState
                  else
                    simulate
                      terminator
                      newState
                        { shapeQueue = tail state.shapeQueue,
                          fallingRock = FallingRock (2, newState.towerHeight + 4) (head state.shapeQueue),
                          indexCombinations = Map.insert (ShapeJetIndexCombination rockIndex jetIndex) (newState.towerHeight, newState.rocksSpawned, newState.jetsSpawned) state.indexCombinations
                        }

maxYCoord :: [Coord] -> Int
maxYCoord = maximum . map snd

rockCoords :: FallingRock -> [Coord]
rockCoords rock =
  let addPairs (a, b) (c, d) = (a + c, b + d)
   in (addPairs rock.position) <$> rock.shape

fallingRockOverlapsWithChamber :: Chamber -> FallingRock -> Bool
fallingRockOverlapsWithChamber chamber rock = or (flip Set.member chamber <$> rockCoords rock)

isOutsideBounds :: FallingRock -> Bool
isOutsideBounds rock = or (isCoordOutOfBounds <$> rockCoords rock)
  where
    isCoordOutOfBounds c = let x = fst c in x < 0 || x > 6

pushRock :: Chamber -> FallingRock -> Jet -> FallingRock
pushRock chamber rock jet =
  let delta = if jet == PushToRight then 1 else -1
      newRock = rock {position = (fst rock.position + delta, snd rock.position)}
   in if isOutsideBounds newRock || fallingRockOverlapsWithChamber chamber newRock then rock else newRock

fallDown :: Chamber -> FallingRock -> (FallingRock, Bool)
fallDown chamber rock =
  let newRock = rock {position = (fst rock.position, snd rock.position - 1)}
   in if fallingRockOverlapsWithChamber chamber newRock then (rock, False) else (newRock, True)

------------ PART A ------------
partA :: [Jet] -> Void
partA input = error ""

-- partA input =
--   let endState = simulate (\s -> null s.shapeQueue) (initialState input)
--    in --  in printLines $ renderChamber ( endState.chamber)
--       endState.towerHeight

renderChamber :: Int -> State -> DisplayString
renderChamber h state =
  printLines $
    reverse $
      [[if Set.member (x, y) state.chamber then '#' else ' ' | x <- [0 .. 6]] | y <- [h - d .. h + 10]]
  where
    d = 9

------------ PART B ------------
partB input =
  let endState =
        simulate
          ( \state ->
              let combination = ShapeJetIndexCombination state.rockIndex state.jetIndex
               in state.rocksSpawned == 560
              --  in if state.rocksSpawned > 1879 && Map.member combination state.indexCombinations then trace ("previous (height, rocksSpawned, jetsSpawned): " ++ (show $ state.indexCombinations Map.! combination)) True else False
          )
          (initialState input)
   in renderChamber 296 $ traceShow (renderChamber endState.towerHeight endState) spy endState

-- repeat = ((1000000000000 - 198) // (1888 - 198)) * (2943 - 296) = 1566272188472
-- restant = f(((1000000000000 - 198) mod (1888 - 198)) + 198) = f(560) = 880
