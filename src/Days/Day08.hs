module Days.Day08 (runDay) where

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
import Util.Parsers (around)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (word `sepBy1` char ' ' `around` string " | ") `sepBy1` endOfLine
  where
    word = Set.fromList <$> many1 letter

------------ TYPES ------------
type Display = ([Set Char], [Set Char])

type Input = [Display]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter (`elem` [2, 3, 4, 7]) . fmap Set.size . concatMap snd

------------ PART B ------------
deduce :: Display -> Int
deduce (signals, output) =
  let -- First we identify the digits with a unique number of segments.
      one = head . filter ((== 2) . length) $ signals
      seven = head . filter ((== 3) . length) $ signals
      four = head . filter ((== 4) . length) $ signals
      eight = head . filter ((== 7) . length) $ signals

      -- Next, we notice that of the digits with five segments, three is the only one which fully contains the digit 1.
      three =
        head . filter ((== 2) . length . (`Set.intersection` one))
          . filter ((== 5) . length)
          $ signals

      -- Next we handle the digits with six segments.
      -- 6 is the only digit missing a segment which is present in 1.
      -- 0 only contains three of the segments in 4. 6 does too, but we've found it already, so we can rule it out.
      -- 9 is the only remaining digit with six segments.
      six =
        head . filter ((== 1) . length . (`Set.intersection` one))
          . filter ((== 6) . length)
          $ signals
      zero =
        head . filter (/= six) . filter ((== 3) . length . (`Set.intersection` four))
          . filter ((== 6) . length)
          $ signals
      nine =
        head . filter (`notElem` [six, zero])
          . filter ((== 6) . length)
          $ signals

      -- Finally, 2 and 5 are differentiable by their overlap with the digit 4.
      -- 2 overlaps with 4 in only two places (5 and 3 overlap with 4 in three places).
      -- Finally, 5 is the remaining digit with five segments.
      two =
        head . filter ((== 2) . length . (`Set.intersection` four))
          . filter ((== 5) . length)
          $ signals
      five = head . filter (`notElem` [two, three]) . filter ((== 5) . length) $ signals

      segmentMap =
        Map.fromList . flip zip [0 .. 9] $
          [zero, one, two, three, four, five, six, seven, eight, nine]
   in foldl1' (\a x -> a * 10 + x) $ (segmentMap Map.!) <$> output

partB :: Input -> OutputB
partB = sum . fmap deduce
