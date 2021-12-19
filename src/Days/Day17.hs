module Days.Day17 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( nub )
import Util.Parsers (around)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( decimal, signed, string, Parser )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  string "target area: x="
  xs <- signed decimal `around` string ".."
  string ", y="
  ys <- signed decimal `around` string ".."
  return (xs, ys)

------------ TYPES ------------
type Input = ((Int, Int), (Int, Int))

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (_, (minY, _)) = triangle minY

------------ PART B ------------
triangle :: Int -> Int
triangle n = n * (n + 1) `div` 2

-- Computes the "inverse triangle number" of a value
-- For non-triangular input values the result will be *between* real numbers
inverseTriangle :: Floating a => Int -> a
inverseTriangle n = sqrt (2 * fromIntegral n + (1 / 4)) - 1 / 2

-- Computes the list of velocities which reach the target area after n steps
-- Some intuitions:
-- - The x and y velocities can be treated independently
-- - Therefore, we only need to find the initial x velocities which will be in the target range after n steps
-- - ...and the initial y velocities which will be in the target range after n steps
--
-- So, how to do this? Well, let's start with the y values:
-- After n steps, the y coordinate will be nY - T_{n-1}, where Y is the starting Y velocity and T_{n-1} is the (n-1)th triangle number
-- Rearranging the inequalities, solving for Y, we want a y value in:
-- [(minY + T_{n-1}) / n; (maxY + T_{n-1}) / n]
-- (and of course, only integral values in this range)
--
-- We can use a similar approach for x values, but more care is required
-- This is because the closed form of nX - T_{n-1} no longer applies
-- Once n > X, the x velocity remains 0 (it doesn't go negative)
-- Therefore if n is high enough we need only consider the cases when the probe comes to a full stop on the x axis before hitting the target
-- This can happen at any trianglular x-coordinate in our input, so we just pick those.
afterSteps :: Input -> Int -> [(Int, Int)]
afterSteps ((minX, maxX), (minY, maxY)) n =
  let triangleNumbersInXRange =
        [ceiling (inverseTriangle minX) .. floor (inverseTriangle maxX)]
      xValues =
        if n <= head triangleNumbersInXRange
          then
            [ ceiling (fromIntegral (minX + triangle (n - 1)) / fromIntegral n)
              .. floor (fromIntegral (maxX + triangle (n - 1)) / fromIntegral n)
            ]
          else triangleNumbersInXRange
      yValues =
        [ ceiling (fromIntegral (minY + triangle (n - 1)) / fromIntegral n)
          .. floor (fromIntegral (maxY + triangle (n - 1)) / fromIntegral n)
        ]
   in (,) <$> xValues <*> yValues

-- We need to nub the values we get from afterSteps
-- This is because some initial velocities can remain in the target for multiple steps, so they'll be double-counted
-- Also, we choose a final value of 2 * abs minY, as this is the maximum number of steps that allow the probe to land (per Part A)
partB :: Input -> OutputB
partB input@(_, (minY, _)) =
  length . nub . concatMap (afterSteps input) $
    [1 .. (2 * abs minY)]
