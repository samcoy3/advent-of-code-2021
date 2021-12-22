module Days.Day21 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Function (on)
import Data.List ( foldl', sortOn )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Util.Pair ( (<+>), Pair(..) )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( endOfLine, decimal, string, Parser )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  string "Player 1 starting position: "
  x <- decimal
  endOfLine
  string "Player 2 starting position: "
  y <- decimal
  return (x, y)

------------ TYPES ------------
type Input = (Int, Int)

-- A representation of a game state
-- The first player in each pair is always to-move
data Game = Game
  { scores :: Input,
    positions :: (Int, Int)
  }
  deriving (Eq, Show, Ord)

type QuantumMap = Map Game (Pair Int)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Plays the game with the deterministic D100 until a winner is found
-- Returns the final game state and number of rolls
playGame :: [Int] -> Int -> Game -> (Game, Int)
playGame die rolls game =
  if (>= 1000) . snd . scores $ game
    then (game, rolls)
    else
      let dieRolls = take 3 die
          newDie = drop 3 die
          newPos = (+ 1) . (`mod` 10) . (+ sum dieRolls) . subtract 1 $ (fst . positions $ game)
       in playGame newDie (rolls + 3) $
            game
              { scores = fmap (+ newPos) . swap $ scores game,
                positions = newPos <$ swap (positions game)
              }

partA :: Input -> OutputA
partA =
  (\(game, rolls) -> fst (scores game) * rolls)
    . playGame (cycle [1 .. 100]) 0
    . Game (0, 0)

------------ PART B ------------
sortPairsBySum :: [(Int, Int)] -> [(Int, Int)]
sortPairsBySum = sortOn (negate . uncurry (+))

swapPair :: Pair Int -> Pair Int
swapPair = Pair . swap . getPair

-- Using dynamic programming, given a winning score,
-- computes a map from the set of game states with scores lower than the winning score to the number of universes in which each player wins
computeUniverses :: Int -> QuantumMap
computeUniverses n = foldl' addScoreToMap Map.empty (sortPairsBySum $ (,) <$> [0 .. (n -1)] <*> [0 .. (n -1)])
  where
    addScoreToMap qmap score =
      foldl' addGameToMap qmap
        . fmap (Game score)
        $ (,) <$> [1 .. 10] <*> [1 .. 10]
    addGameToMap qmap game =
      -- These are the possible outcomes of the rolls
      let possibleRollsWithWeights = zip [3, 4, 5, 6, 7, 8, 9] [1, 3, 6, 7, 6, 3, 1]
       in (\n -> Map.insert game n qmap)
            . foldr1 (<+>)
            . fmap swapPair
            . flip fmap possibleRollsWithWeights
            $ \(roll, weight) ->
              (* weight)
                <$> lookupGameInMap qmap (applyRollToGame game roll)
    applyRollToGame game roll =
      let newPos = (+ 1) . (`mod` 10) . (+ roll) . subtract 1 $ (fst . positions $ game)
       in game
            { scores = fmap (+ newPos) . swap $ scores game,
              positions = newPos <$ swap (positions game)
            }
    -- We only need to check the second player's score here, because we only care about the possible results *the turn after* the die is rolled
    lookupGameInMap qmap game =
      if ((>= n) . snd . scores) game
        then Pair (0, 1)
        else qmap Map.! game

partB :: Input -> OutputB
partB p = uncurry max . getPair . (Map.! Game (0, 0) p) $ computeUniverses 21
