module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( sort )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( decimal, sepBy, char, Parser )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA crabs =
  let median = sort crabs !! (length crabs `div` 2)
   in sum $ fmap (abs . subtract median) crabs

------------ PART B ------------
partB :: Input -> OutputB
partB crabs =
  let mean = sum crabs `div` length crabs
      fuelUsage distance = (distance * (distance + 1)) `div` 2
   in min
        (sum $ fmap (fuelUsage . abs . subtract mean) crabs)
        (sum $ fmap (fuelUsage . abs . subtract (mean + 1)) crabs)
