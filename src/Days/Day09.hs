module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Bifunctor (bimap)
import Data.List ( sortOn )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe ( mapMaybe )
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (endOfLine, sepBy1, digit, many1, Parser)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser =
  U.mapFromNestedLists
    <$> many1 (read . pure <$> digit) `sepBy1` endOfLine

------------ TYPES ------------
type Input = Map (Int, Int) Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = bimap (x +) (y +) <$> [(1, 0), (0, 1), (-1, 0), (0, -1)]

-- Finds the low points in the input and returns a Map corresponding to them.
lowPoints :: Input -> Input
lowPoints input = Map.filterWithKey lowPoints' input
  where
    lowPoints' loc height =
      all (> height)
        . mapMaybe (input Map.!?)
        $ neighbours loc

partA :: Input -> OutputA
partA = sum . fmap (+ 1) . lowPoints

------------ PART B ------------
partB :: Input -> OutputB
partB input = product . take 3 . sortOn negate . fmap (length . flood) . Map.keys $ lowPoints input
  where
    -- We use a simple flooding algorithm to find the sizes of the basins.
    -- In each iteration, we have a basin and a basin edge.
    -- The basin edge is the "frontier" of the basin and is used to expand it.
    -- Once the basin edge no longer finds anything, we know there is no more to find.
    flood loc = flood' (Set.singleton loc) (Set.singleton loc)
    flood' basin basinEdge =
      let basinEdge' =
            (Set.\\ basin)
              . Set.filter ((/= 9) . (input Map.!))
              . Set.filter (`Map.member` input)
              . Set.unions
              . Set.map (Set.fromList . neighbours)
              $ basinEdge
       in if Set.null basinEdge'
            then basin
            else flood' (Set.union basin basinEdge') basinEdge'
