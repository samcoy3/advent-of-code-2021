module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Char (isLower)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Util.Parsers (around)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser, choice, many1, endOfInput, letter, char, endOfLine )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
-- Construct an adjacency list from our input
-- Note: it's important to delete incoming edges to "start":
--   we never want to take them (including in Part B)
inputParser :: Parser Input
inputParser = fmap (Set.delete "start") <$>
  choice
    [ endOfInput $> Map.empty,
      edge <* endOfLine >>= \(u, v) ->
        Map.insertWith Set.union u (Set.singleton v)
          . Map.insertWith Set.union v (Set.singleton u)
          <$> inputParser
    ]
  where
    edge = many1 letter `around` char '-'

------------ TYPES ------------
type Input = Graph
type Graph = Map Node (Set Node)
type Node = String

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- A parallel "bfs"-like algorithm for path enumeration.
bfs :: Bool -> Graph -> [[Node]]
bfs canVisitSmallCave graph = bfs' canVisitSmallCave Set.empty "start"
  where
    bfs' canVisitSmallCave excluded current =
          let newExcluded = if isLower (head current) then Set.insert current excluded else excluded
              validNeighbours = (graph Map.! current) Set.\\ excluded
              secondSmallCaves = if canVisitSmallCave
                then (graph Map.! current) `Set.intersection` excluded
                else Set.empty
           in if current == "end"
                 then [["end"]]
                 else fmap (current :) . concat
                      $ (bfs' canVisitSmallCave newExcluded
                      <$> Set.toList validNeighbours)
                      ++ (bfs' False newExcluded
                      <$> Set.toList secondSmallCaves)

partA :: Input -> OutputA
partA = length . bfs False

------------ PART B ------------
partB :: Input -> OutputB
partB = length . bfs True
