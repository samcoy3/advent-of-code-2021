module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( char, sepBy, decimal, Parser )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = U.freq <$> decimal `sepBy` char ','

------------ TYPES ------------
type Input = Map Int Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
step :: Map Int Int -> Map Int Int
step population =
  let parents = Map.findWithDefault 0 0 population
   in Map.delete (-1)
        . Map.insert 8 parents
        . Map.insertWith (+) 6 parents
        . Map.mapKeysMonotonic (subtract 1)
        $ population

afterDays :: Int -> Input -> Int
afterDays days = sum . Map.elems . (!! days) . iterate step

partA :: Input -> OutputA
partA = afterDays 80

------------ PART B ------------
partB :: Input -> OutputB
partB = afterDays 256
