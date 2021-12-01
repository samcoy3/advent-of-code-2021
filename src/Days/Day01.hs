module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
consecPairs :: [a] -> [(a, a)]
consecPairs xs = zip xs (tail xs)

partA :: Input -> OutputA
partA = length . filter (\(a, b) -> a < b) . consecPairs

------------ PART B ------------
consecTriples :: [a] -> [(a, a, a)]
consecTriples xs = zip3 xs (tail xs) (tail (tail xs))

partB :: Input -> OutputB
partB = partA . fmap (\(a, b, c) -> a + b + c) . consecTriples
