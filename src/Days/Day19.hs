module Days.Day19 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( (\\), nub, transpose )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromJust, isJust )
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser, sepBy, decimal, signed, char, endOfLine, string )
import Debug.Trace (traceShowId)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = scanner `sepBy` (endOfLine >> endOfLine)
  where
    scanner =
      string "--- scanner " >> decimal >> " ---" >> endOfLine
        >> (point `sepBy` endOfLine)
    point = do
      x <- signed decimal
      char ','
      y <- signed decimal
      char ','
      z <- signed decimal
      return $ Point x y z

------------ TYPES ------------
data Point = Point {x :: Int, y :: Int, z :: Int} deriving (Eq, Show, Ord)

-- Generates all possible rotations of a point in multiples of 90 degrees around the three axes
generateRotations :: Point -> [Point]
generateRotations (Point x y z) = fmap (\(a, b, c) -> Point a b c) . nub $ do
  (x', y', z') <- [(x, y, z), (- y, x, z), (- x, - y, z), (y, - x, z)]
  (x'', y'', z'') <- [(x', y', z'), (- z', y', x'), (- x', y', - z'), (z', y', - x')]
  (x''', y''', z''') <- [(x'', y'', z''), (x'', - z'', y''), (x'', - y'', - z''), (x'', z'', - y'')]
  return (x''', y''', z''')

-- Vector subtraction
(<->) :: Point -> Point -> Point
(Point a b c) <-> (Point x y z) = Point (a - x) (b - y) (c - z)

type Scanner = [Point]

type Input = [Scanner]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Given two scanners, checks if they overlap
-- If they overlap, returns Just a pair:
-- - The first of the pair is the scanner location (relative to the origin, scanner 0)
-- - The second of the pair is the beacons the scanner sees, normalized to the origin
-- If they don't overlap, returns Nothing
overlaps :: Scanner -> Scanner -> Maybe (Point, Scanner)
overlaps s1 s2 =
  let possibleRotations = transpose $ generateRotations <$> s2
      rotationPairwiseDifferences rotation = U.freq $ (<->) <$> rotation <*> s1
      matches = filter (any (>= 12) . snd) $ fmap rotationPairwiseDifferences . (\x -> (x, x)) <$> possibleRotations
   in if null matches
        then Nothing
        else
          let (rot, match) = head matches
              matchingTranslation = head . Map.keys . Map.filter (>= 12) $ match
           in Just . (matchingTranslation,) $ (<-> matchingTranslation) <$> rot

-- Given the input list of scanners, assembles the map of beacons
-- In the returned list of pairs, the first of each pair is the scanner location
-- The second is the list of (normalized) beacons that it sees
scannerBFS :: [Scanner] -> [(Point, Scanner)]
scannerBFS scanners = bfs [(Point 0 0 0, head scanners)] [head scanners] scanners
  where
    bfs frozen [] _ = frozen
    bfs frozen (e : toExplore) scanners =
      let nextScanners = filter (isJust . (e `overlaps`)) scanners
          nextScannersNormalized = fmap (fromJust . (e `overlaps`)) nextScanners
       in bfs
            (frozen ++ nextScannersNormalized)
            (fmap snd nextScannersNormalized ++ toExplore)
            (scanners \\ nextScanners)

partA :: Input -> OutputA
partA = length . nub . concatMap snd . scannerBFS

------------ PART B ------------
distance :: Point -> Point -> Int
distance (Point a b c) (Point x y z) = abs (a - x) + abs (b - y) + abs (c - z)

partB :: Input -> OutputB
partB input =
  let scanners = fmap fst . scannerBFS $ input
   in maximum . fmap (uncurry distance) $ (,) <$> scanners <*> scanners
