module Days.Day06 (runDay) where

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
import Control.Monad.State (StateT (runStateT), MonadState (put, get), MonadTrans (lift), runState)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = fst <$> runStateT parseTillMarker []

windowSize = 14

parseTillMarker :: StateT [Char] Parser String
parseTillMarker = many' $ do
  currentWindow <- get
  if length currentWindow == windowSize && nub currentWindow == currentWindow
    then fail "Marker reached"
    else do
      nextChar <- lift anyChar
      let newWindow = nextChar : take (windowSize - 1) currentWindow
      put newWindow
      return nextChar

------------ TYPES ------------
type Input = String

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length

------------ PART B ------------
partB :: Input -> OutputB
partB = length
