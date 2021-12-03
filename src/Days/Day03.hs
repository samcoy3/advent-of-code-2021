module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List ( foldl', transpose )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser, many1, sepBy, char, endOfLine )
import Data.Void ()
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (char '1' $> True <|> char '0' $> False) `sepBy` endOfLine

------------ TYPES ------------
type Input = [[Bool]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Returns GT if there is a majority of True values in the input list of Bools;
-- LT if there is a majority of False values; and EQ if there are an equal number.
truesCount :: [Bool] -> Ordering
truesCount bits = compare (length . filter id $ bits) (length . filter not $ bits)

partA :: Input -> OutputA
partA = uncurry (*) . foldl' processColumn (0, 0) . transpose
  where
    processColumn (gamma, epsilon) bits =
      if truesCount bits == GT
        then (gamma * 2 + 1, epsilon * 2)
        else (gamma * 2, epsilon * 2 + 1)

------------ PART B ------------
data RatingType = Oxygen | CO2 deriving (Eq, Show)

partB :: Input -> OutputB
partB report = findRating report Oxygen * findRating report CO2
  where
    findRating = findRating' 0
    findRating' rating report reportType =
      -- Dealing with the case where we've found our line in the report.
      if length report == 1
        then foldl' (\acc bit -> acc * 2 + fromEnum bit) rating $ head report
        else -- If we haven't found our line of the report, we narrow down the report according to our bit criteria.

          let correctBit = applyBitCriteria (head <$> report) reportType
           in findRating'
                (2 * rating + fromEnum correctBit)
                (tail <$> filter ((== correctBit) . head) report)
                reportType

    -- Applies the "bit criteria" to a list of bits.
    -- The correct bit is 1 (true) iff the rating type is Oxygen and there are at least as many 1s as 0s;
    -- or the rating type is CO2 and the 0s strictly outnumber the 1s.
    applyBitCriteria bits rtype =
      (truesCount bits, rtype) `elem` [(GT, Oxygen), (LT, CO2), (EQ, Oxygen)]
