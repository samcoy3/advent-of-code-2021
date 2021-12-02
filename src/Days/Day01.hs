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
partA = length . filter (uncurry (<)) . consecPairs

------------ PART B ------------
-- Computes the sums of each consecutive triple of numbers in a list
slidingWindowSums :: (Num a) => [a] -> [a]
slidingWindowSums xs = zipWith3 (\x y z -> x + y + z) xs (tail xs) (tail (tail xs))

partB :: Input -> OutputB
partB = partA . slidingWindowSums
