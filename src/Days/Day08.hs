module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( foldl1' )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Util.Parsers (around)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser, many1, sepBy1, letter, char, endOfLine, string )
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
  let condition filters = head . filters $ signals

      -- First we identify the digits with a unique number of segments.
      one = condition $ filter ((== 2) . length)
      seven = condition $ filter ((== 3) . length)
      four = condition $ filter ((== 4) . length)
      eight = condition $ filter ((== 7) . length)

      -- Next, we notice that of the digits with five segments, three is the only one which fully contains the digit 1.
      three =
        condition $
          filter ((== 2) . length . (`Set.intersection` one))
            . filter ((== 5) . length)

      -- Next we handle the digits with six segments.
      -- 6 is the only digit missing a segment which is present in 1.
      -- 0 only contains three of the segments in 4. 6 does too, but we've found it already, so we can rule it out.
      -- 9 is the only remaining digit with six segments.
      six =
        condition $
          filter ((== 1) . length . (`Set.intersection` one))
            . filter ((== 6) . length)

      zero =
        condition $
          filter (/= six)
            . filter ((== 3) . length . (`Set.intersection` four))
            . filter ((== 6) . length)

      nine =
        condition $
          filter (`notElem` [six, zero])
            . filter ((== 6) . length)

      -- Finally, 2 and 5 are differentiable by their overlap with the digit 4.
      -- 2 overlaps with 4 in only two places (5 and 3 overlap with 4 in three places).
      -- Finally, 5 is the remaining digit with five segments.
      two =
        condition $
          filter ((== 2) . length . (`Set.intersection` four))
            . filter ((== 5) . length)

      five =
        condition $
          filter (`notElem` [two, three])
            . filter ((== 5) . length)

      segmentMap =
        Map.fromList . flip zip [0 .. 9] $
          [zero, one, two, three, four, five, six, seven, eight, nine]
   in foldl1' (\a x -> a * 10 + x) $ (segmentMap Map.!) <$> output

partB :: Input -> OutputB
partB = sum . fmap deduce
