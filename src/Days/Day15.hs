module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Util.Util as U
import Util.Parsers ( coordinateParser )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( Parser )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (pure . read . pure) 0

------------ TYPES ------------
type Cave = Map (Int, Int) Int

type Input = Cave

type OutputA = Int

type OutputB = Int

------------ PART A ------------
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- We perform Dijkstra's algorithm in order to find the shortest path
dijkstras :: Cave -> Int
dijkstras cave = dijkstras' (Set.singleton (0, (0, 0))) Set.empty
  where
    (_, maxX, _, maxY) = U.mapBoundingBox cave
    -- Candidates is a set of (distance, location) pairs
    -- Ossified is a set of locations which we've "frozen"
    -- Each step, we take the shortest distance of all the candidates and freeze it
    -- We then add its neighbours to the set of candidates
    -- If the next candidate is the final point, great! We have our answer. If not, we recurse.
    dijkstras' candidates ossified =
      let (weight, nextCandidate) = Set.findMin candidates
          newCandidates =
            Set.fromList
              . fmap (\loc -> (weight + (cave Map.! loc), loc))
              . filter (`Set.notMember` ossified)
              . filter (`Map.member` cave)
              $ neighbours nextCandidate
       in if nextCandidate == (maxX, maxY)
            then weight
            else
              dijkstras'
                (Set.union newCandidates (Set.filter ((/= nextCandidate) . snd) candidates))
                (Set.insert nextCandidate ossified)

partA :: Input -> OutputA
partA = dijkstras

------------ PART B ------------
-- Creates the "bigger grid" as per the second half of the question
tileCave :: Cave -> Cave
tileCave cave =
  let (_, maxX, _, maxY) = U.mapBoundingBox cave
      tileShifts = (,) <$> [0 .. 4] <*> [0 .. 4]
      tile (x, y) =
        Map.mapKeys (\(x', y') -> (x' + (x * (maxX + 1)), y' + (y * (maxY + 1))))
          -- quick maffs
          -- (handles the not-quite-modular risk adjustments)
          . fmap ((+ 1) . (`mod` 9) . subtract 1 . (+ x) . (+ y))
          $ cave
   in Map.unions $ tile <$> tileShifts

partB :: Input -> OutputB
partB = dijkstras . tileCave
