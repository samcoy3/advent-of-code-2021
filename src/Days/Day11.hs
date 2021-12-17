module Days.Day11 (runDay) where

{- ORMOLU_DISABLE -}
{- ORMOLU_DISABLE -}
import Data.List ( (\\), elemIndex)
import Data.List.NonEmpty ( toList, unfoldr )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Util.Parsers as P ( coordinateParser )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( Parser )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = P.coordinateParser (pure . Energy . read . pure) 0

------------ TYPES ------------
data EnergyLevel = Energy Int | Flashed deriving (Show, Eq)

type Input = Map (Int, Int) EnergyLevel

type OutputA = Int

type OutputB = Int

------------ PART A ------------
increaseEnergy :: EnergyLevel -> EnergyLevel
increaseEnergy (Energy x) = if x < 9 then Energy (x + 1) else Flashed
increaseEnergy Flashed = Flashed

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) =
  [ (x', y')
    | x' <- [x -1 .. x + 1],
      y' <- [y -1 .. y + 1],
      (x, y) /= (x', y')
  ]

step :: Input -> (Int, Input)
step cavern =
  let -- Reset flashes at the end of a step
      resetFlashes o = if o == Flashed then Energy 0 else o
      -- At the start of the step, increases the energy of all octopodes
      increasedAllEnergy = increaseEnergy <$> cavern
      -- The flashes which are present after increasing the energy levels
      startingFlashes =
        Set.fromList . Map.keys $
          Map.filter (== Flashed) increasedAllEnergy
      -- A flooding algorithm to propagate the flashing.
      -- seenFlashes is a set containing all the nodes that have flashed this step.
      -- recentFlashes only contains nodes which flashed in the most recent epoch of the flooding algorithm.
      step' seenFlashes recentFlashes cavern =
        if Set.null recentFlashes
          then (length seenFlashes, resetFlashes <$> cavern)
          else
            let triggerFlashes = foldr (Map.adjust increaseEnergy) cavern (concat $ Set.map neighbours recentFlashes)
                flashes = Set.fromList . Map.keys $ Map.filter (== Flashed) triggerFlashes
             in step' flashes (flashes Set.\\ seenFlashes) triggerFlashes
   in step' startingFlashes startingFlashes increasedAllEnergy

partA :: Input -> OutputA
partA = sum . take 100 . toList . unfoldr (fmap pure . step)

------------ PART B ------------
partB :: Input -> OutputB
partB = (+ 1) . fromJust . elemIndex 100 . toList . unfoldr (fmap pure . step)
