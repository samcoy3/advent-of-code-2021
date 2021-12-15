module Days.Day13 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.Function (on)
import Data.Functor (($>))
import Data.List ( foldl', intercalate )
import Data.Set (Set)
import qualified Data.Set as Set
import Util.Parsers (around)

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser, count, sepBy, decimal, char, endOfLine, string )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> (dots <* count 2 endOfLine) <*> folds
  where
    dots =
      Set.fromList
        <$> (decimal `around` char ',' `sepBy` endOfLine)
    folds =
      ( string "fold along "
          *> ( (,)
                 <$> (char 'x' $> Horizontal <|> char 'y' $> Vertical)
                 <*> (char '=' *> decimal)
             )
      )
        `sepBy` endOfLine

------------ TYPES ------------
data Direction = Horizontal | Vertical deriving (Eq, Show)
type Fold = (Direction, Int)

type Input = (Set (Int, Int), [Fold])

type OutputA = Int

type OutputB = String

------------ PART A ------------
performFold :: Set (Int, Int) -> Fold -> Set (Int, Int)
performFold points (direction, threshold) = flip Set.map points $
  \(x, y) -> if
    | direction == Vertical && y < threshold -> (x, y)
    | direction == Horizontal && x < threshold -> (x, y)
    | direction == Vertical && y > threshold
      -> (x, 2 * threshold - y)
    | direction == Horizontal && x > threshold
      -> (2 * threshold - x, y)

partA :: Input -> OutputA
partA (points, folds) = Set.size $ performFold points (head folds)

------------ PART B ------------
instance {-# OVERLAPS #-} Show String where
  show = id

printDots :: Set (Int, Int) -> String
printDots points =
  let (minX, maxX, minY, maxY) =
        (,,,)
        (minimum (Set.map fst points))
        (maximum (Set.map fst points))
        (minimum (Set.map snd points))
        (maximum (Set.map snd points))
      display y x = if (x, y) `Set.member` points
        then 'X'
        else ' '
      row y = display y <$> [minX .. maxX]
   in intercalate "\n" (row <$> [minY .. maxY])

partB :: Input -> OutputB
partB (points, folds) = printDots $ foldl' performFold points folds
