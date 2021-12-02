module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = ((,) <$> parseDirection <*> (skipSpace *> decimal)) `sepBy` endOfLine
  where
    parseDirection =
      string "forward" $> Forward
        <|> string "up" $> Up
        <|> string "down" $> Down

------------ TYPES ------------
data Direction = Forward | Up | Down deriving (Show, Eq)

type Instruction = (Direction, Int)

type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- We track our horizontal co-ordinate (h) and depth (v)
partA :: Input -> OutputA
partA = (\(h, v) -> h * v) . foldl' executeInstruction (0, 0)
  where
    executeInstruction (h, v) (direction, magnitude) = case direction of
      Forward -> (h + magnitude, v)
      Up -> (h, v - magnitude)
      Down -> (h, v + magnitude)

------------ PART B ------------
-- Here we also track aim (a)
partB :: Input -> OutputB
partB = (\(h, v, _) -> h * v) . foldl' executeInstruction (0, 0, 0)
  where
    executeInstruction (h, v, a) (direction, magnitude) = case direction of
      Forward -> (h + magnitude, v + (magnitude * a), a)
      Up -> (h, v, a - magnitude)
      Down -> (h, v, a + magnitude)
