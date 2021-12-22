module Days.Day20 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List ( foldl' )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Util.Parsers ( coordinateParser )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( Parser, many1, char, endOfLine )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> algorithm <* (endOfLine >> endOfLine) <*> image
  where
    algorithm = Vec.fromList <$> many1 (char '#' $> True <|> char '.' $> False)
    image = Set.fromList . fmap swap . Map.keys . Map.filter id <$> coordinateParser pixel 0
    pixel '#' = Just True
    pixel '.' = Just False
    pixel _ = Nothing

------------ TYPES ------------
type Input = (Vector Bool, Set (Int, Int))

type OutputA = Int

type OutputB = Int

------------ PART A ------------
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x', y') | x' <- [x -1 .. x + 1], y' <- [y -1 .. y + 1]]

-- Returns the minimum x, maximum x, minimum y, and maximum y values in the set
boundingBox :: Set (Int, Int) -> (Int, Int, Int, Int)
boundingBox s =
  let xs = Set.map fst s
      ys = Set.map snd s
   in (,,,)
        (minimum xs)
        (maximum xs)
        (minimum ys)
        (maximum ys)

-- Checks whether a point is inside a box
insideBox :: (Int, Int, Int, Int) -> (Int, Int) -> Bool
insideBox (minX, maxX, minY, maxY) (x, y) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

-- Applies the algorithm value to a single pixel
applyAlgorithmToPixel ::
  Vector Bool ->
  Set (Int, Int) ->
  Bool ->
  (Int, Int, Int, Int) ->
  (Int, Int) ->
  Bool
applyAlgorithmToPixel alg image abyss box pixel =
  (alg Vec.!)
    . foldl' (\a x -> a * 2 + fromEnum x) 0
    . fmap (\p -> if insideBox box p then p `Set.member` image else abyss)
    $ neighbours pixel

-- We note that, as a feature of our input, when a dark pixel is surrounded by dark pixels it becomes light, and vice versa
-- Therefore the infinite abyss will flash each step, and we have to account for this
-- We track a "default value" for the infinite grid in the variable <abyss>
steps :: Int -> Vector Bool -> Set (Int, Int) -> Set (Int, Int)
steps n = steps' False 0
  where
    steps' abyss n' alg image =
      let box@(minX, maxX, minY, maxY) = boundingBox image
       in if n' == n
            then image
            else
              steps' (not abyss) (n' + 1) alg $
                Set.fromList
                  [ (x', y')
                    | x' <- [minX -1 .. maxX + 1],
                      y' <- [minY -1 .. maxY + 1],
                      applyAlgorithmToPixel alg image abyss box (x', y')
                  ]

partA :: Input -> OutputA
partA = Set.size . uncurry (steps 2)

------------ PART B ------------
partB :: Input -> OutputB
partB = Set.size . uncurry (steps 50)
