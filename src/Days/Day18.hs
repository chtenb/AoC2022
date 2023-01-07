{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
module Days.Day18 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List hiding (union)
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
import Util.Util
import Data.UnionFind.ST
import Control.Monad.ST
import Control.Monad (filterM)
import Control.Monad.Identity (Identity(Identity, runIdentity))
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseCube `sepBy` "\n"
  where
    parseCube = do
      x <- decimal
      string ","
      y <- decimal
      string ","
      z <- decimal
      return (x, y, z)

------------ TYPES ------------
type Input = [Cube]

type Cube = (Int, Int, Int)

type Droplet = Set Cube

------------ PART A ------------
partA :: Input -> Int
partA input =
  let cubeSet = Set.fromList input
      isSurfaceExterior :: Cube -> Identity Bool = return . not . flip Set.member cubeSet
   in runIdentity $ do
        surfaces <- mapM (countSurface isSurfaceExterior) input
        return $ sum surfaces

countSurface :: Applicative m => (Cube -> m Bool) -> Cube -> m Int
countSurface isSurfaceExterior cube = length <$> filterM isSurfaceExterior (cubeNeighbors cube)

cubeNeighbors :: Cube -> [Cube]
cubeNeighbors (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dz <- [-1, 0, 1], abs dx + abs dy + abs dz == 1]

------------ PART B ------------
-- partB :: Input -> Void
partB input = runST $ do
  let box = boundingBox input
  let droplet = Set.fromList input
  pointMap <- initialPoints box
  computeConnectedComponents input pointMap
  let isSurfaceExterior cube = equivalent (pointMap Map.! (0, 0, 0)) (pointMap Map.! cube)
  surfaces <- mapM (countSurface isSurfaceExterior) input
  return $ sum surfaces

data BoundingBox = BoundingBox {xMin :: Int, xMax :: Int, yMin :: Int, yMax :: Int, zMin :: Int, zMax :: Int} deriving (Show)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

boundingBox :: Input -> BoundingBox
boundingBox input =
  let xMin = -1 + minimum (map fst3 input)
      xMax = 1 + maximum (map fst3 input)
      yMin = -1 + minimum (map snd3 input)
      yMax = 1 + maximum (map snd3 input)
      zMin = -1 + minimum (map thd3 input)
      zMax = 1 + maximum (map thd3 input)
   in BoundingBox {xMin, xMax, yMin, yMax, zMin, zMax}

inBoundingBox :: BoundingBox -> Cube -> Bool
inBoundingBox box (x, y, z) =
  box.xMin <= x
    && x <= box.xMax
    && box.yMin <= y
    && y <= box.yMax
    && box.zMin <= z
    && z <= box.zMax

cubesInBoundingBox :: BoundingBox -> [Cube]
cubesInBoundingBox box = [(x, y, z) | x <- [box.xMin .. box.xMax], y <- [box.yMin .. box.yMax], z <- [box.zMin .. box.zMax]]

initialPoints :: BoundingBox -> ST s (Map Cube (Point s Cube))
initialPoints box = do
  list <-
    mapM
      ( \cube -> do
          point <- fresh cube
          return (cube, point)
      )
      (cubesInBoundingBox box)
  return $ Map.fromList list

computeConnectedComponents :: Input -> Map Cube (Point s Cube) -> ST s ()
computeConnectedComponents input pointMap =
  let box = boundingBox input
      droplet = Set.fromList input
   in do
        mapM_ (mergeNeighbors box droplet pointMap) (Map.assocs pointMap)
  where
    mergeNeighbors :: BoundingBox -> Droplet -> Map Cube (Point s Cube) -> (Cube, Point s Cube) -> ST s ()
    mergeNeighbors box droplet pointMap (cube, point) =
      let neighborsInComponent = filter (\n -> Set.member n droplet == Set.member cube droplet && inBoundingBox box n) $ cubeNeighbors cube
          neighborPoints = (Map.!) pointMap <$> neighborsInComponent
       in mapM_ (union point) neighborPoints
