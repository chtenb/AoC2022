module Days.Day07 (runDay) where

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
import Control.Applicative ((<|>))
import Data.Functor ((<&>), ($>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = crawlDown

crawlDown = do
  string "$ cd "
  dirName <- manyTill anyChar (string "\n")
  if dirName == ".."
    then fail "crawl up"
    else do
      string "$ ls\n"
      dirOutput <- many' (parseDir <|> parseFile)
      let files = catMaybes dirOutput
      subTrees <- manyTill crawlDown ((string "$ cd ..\n" $> ()) <|> endOfInput)
      return $ Dir dirName files subTrees

parseDir = do
  string "dir "
  dirname <- manyTill anyChar (string "\n")
  return Nothing

parseFile = do
  fileSize <- decimal
  string " "
  fileName <- manyTill anyChar (string "\n")
  return $ Just $ File fileName fileSize

------------ TYPES ------------
type Input = Dir

type OutputA = Int

type OutputB = Int

data Dir = Dir String [File] [Dir] deriving (Show)

data File = File String Int deriving (Show)

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . filter (<= 100000) . map snd . allSizes . getDirectorySizes

data DirSizeResult = DirSizeResult {totalSize :: Int, allSizes :: [(String, Int)]} deriving (Show)

emptyResult = DirSizeResult {totalSize = 0, allSizes = []}

getDirectorySizes :: Dir -> DirSizeResult
getDirectorySizes (Dir name files subDirs) =
  DirSizeResult
    { totalSize = totalSize,
      allSizes = (name, totalSize) : subResultAcc.allSizes
    }
  where
    fileSizeSum = (sum . map (\(File _ size) -> size)) files
    recResults = map getDirectorySizes subDirs
    subResultAcc =
      foldr
        ( \sub acc ->
            DirSizeResult
              { totalSize = acc.totalSize + sub.totalSize,
                allSizes = sub.allSizes ++ acc.allSizes
              }
        )
        emptyResult
        recResults
    totalSize = fileSizeSum + subResultAcc.totalSize

------------ PART B ------------
totalSpace = 70000000

requiredSpace = 30000000

partB :: Input -> OutputB
partB input =
  let DirSizeResult {totalSize, allSizes} = getDirectorySizes input
      usedSpace = totalSize
      unusedSpace = totalSpace - usedSpace
      spaceToFreeUp = requiredSpace - unusedSpace
   in (minimum . filter (>= spaceToFreeUp) . map snd) allSizes
