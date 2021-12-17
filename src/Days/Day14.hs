module Days.Day14 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( endOfLine, sepBy1, many1, Parser, count, letter, string )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> many1 letter <* count 2 endOfLine <*> rules
  where
    rules = Map.fromList <$> rule `sepBy1` endOfLine
    rule = do
      left <- letter
      right <- letter
      string " -> "
      middle <- letter
      return ((left, right), middle)

------------ TYPES ------------
-- Represents the rules for the input: maps pairs of elements to the elements that pair should have inserted
type InsertionRules = Map (Char, Char) Char

-- Represents how often consective pairs of elements appear
type PairFrequencies = Map (Char, Char) Int

type Input = ([Char], InsertionRules)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Given the pair update rules and a frequency map of pairs,
-- apply the rules to those pairs (i.e. perform one "step")
applyRules :: InsertionRules -> PairFrequencies -> PairFrequencies
applyRules rules = Map.foldrWithKey addNewPairs Map.empty
  where
    addNewPairs (l, r) quant =
      Map.insertWith (+) (l, rules Map.! (l, r)) quant
        . Map.insertWith (+) (rules Map.! (l, r), r) quant

-- Takes a number of steps, initial polymer, and the pair update rules
-- Returns the difference between the most and least common elements in the polymer after the specified number of steps of update rules
frequencyRangeAfterSteps :: Int -> [Char] -> InsertionRules -> Int
frequencyRangeAfterSteps steps polymer rules =
  let -- The initial frequency of pairs in the polymer
      initialPairFreqs = U.freq $ zip polymer (tail polymer)
      -- The frequency of pairs in the polymer, after the specified number of steps
      finalPairFreqs = iterate (applyRules rules) initialPairFreqs !! steps
      -- The frequency of each element in the pairs
      elementFreqs = Map.foldrWithKey insertBothElements Map.empty finalPairFreqs
        where
          insertBothElements (l, r) quant =
            Map.insertWith (+) l quant
              . Map.insertWith (+) r quant
      -- NOTE: We've double-counted all elements but the first and last, so we update the map accordingly
      normalisedElementFreqs =
        fmap (`div` 2)
          . Map.adjust (+ 1) (head polymer)
          . Map.adjust (+ 1) (last polymer)
          $ elementFreqs
   in maximum normalisedElementFreqs - minimum normalisedElementFreqs

partA :: Input -> OutputA
partA = uncurry (frequencyRangeAfterSteps 10)

------------ PART B ------------
partB :: Input -> OutputB
partB = uncurry (frequencyRangeAfterSteps 40)
