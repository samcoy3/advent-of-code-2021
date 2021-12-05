module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.Map.Strict as Map
import qualified Util.Util as U
import Util.Parsers (around)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( string, char, decimal, endOfLine, sepBy1, Parser )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = line `sepBy1` endOfLine
  where
    line =
      (,)
        <$> (decimal `around` char ',') <* string " -> "
        <*> (decimal `around` char ',')

------------ TYPES ------------
type Line = ((Int, Int), (Int, Int))

type Input = [Line]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Tests whether a line is orthogonal
isOrthogonal :: Line -> Bool
isOrthogonal ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

-- Given a line, generates a list of points on that line.
-- Note: the question promises that the lines are all orthogonal or diagonal.
points :: Line -> [(Int, Int)]
points line@((x1, y1), (x2, y2)) =
  if isOrthogonal line
    then (,) <$> range x1 x2 <*> range y1 y2
    else zip (range x1 x2) (range y1 y2)
  where
    range a b = if a < b then [a .. b] else [a, a -1 .. b]

-- Finds the number of overlapping points, given a list of lines.
findNumberOfOverlappingPoints :: [Line] -> Int
findNumberOfOverlappingPoints =
  Map.size
    . Map.filter (> 1)
    . U.freq
    . concatMap points

partA :: Input -> OutputA
partA = findNumberOfOverlappingPoints . filter isOrthogonal

------------ PART B ------------
partB :: Input -> OutputB
partB = findNumberOfOverlappingPoints
