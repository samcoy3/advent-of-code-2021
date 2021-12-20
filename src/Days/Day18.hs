module Days.Day18 (runDay) where

{- ORMOLU_DISABLE -}
import Prelude hiding (unzip)

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (maximumBy)
import Data.List.NonEmpty (unzip)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Control.Applicative.Combinators (between)
import Util.Parsers (around)
import Data.List (foldl1')
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = tree `sepBy1` endOfLine
  where
    tree =
      (Regular <$> decimal)
        <|> between
          (char '[')
          (char ']')
          (uncurry Pair <$> (tree `around` char ','))

------------ TYPES ------------
-- A tree representing a snailfish number
data SnailTree a
  = Regular a
  | Pair (SnailTree a) (SnailTree a)
  deriving (Eq, Show)

-- A crumb provides the context necessary to traverse back up the tree
-- For example, LeftCrumb means we went left at this point
-- (and we attach the right tree to the crumb, allowing us to rebuild the tree)
data Crumb a = LeftCrumb (SnailTree a) | RightCrumb (SnailTree a)

-- Given a Pair which is an immediate parent of two leaves, get those leaves (Nothing if this is not the case)
unwrapPair :: SnailTree a -> Maybe (a, a)
unwrapPair (Regular a) = Nothing
unwrapPair (Pair l r) = case (l, r) of
  (Regular x, Regular y) -> Just (x, y)
  _ -> Nothing

type Input = [SnailTree Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Takes a SnailTree and returns either Just another SnailTree (if an explosion happened), or Nothing if no explosions are possible
explode :: SnailTree Int -> Maybe (SnailTree Int)
explode = explode' 0 []
  where
    explode' _ _ (Regular a) = Nothing -- Regular numbers can't explode
    explode' d crumbs p@(Pair l r) =
      if d >= 4 && isJust (unwrapPair p)
        then Just $ rebuildTree crumbs (unzip (unwrapPair p)) (Regular 0)
        else
          explode' (d + 1) (LeftCrumb r : crumbs) l
            <|> explode' (d + 1) (RightCrumb l : crumbs) r

    -- rebuildTree takes a series of crumbs, the left and right values from the explosion, and an accumulatory tree
    -- We need to add the left and right values from the explosion to their "neighbours" if they have any
    rebuildTree [] _ tree = tree
    rebuildTree (c : crumbs) (l, r) tree =
      case (c, l, r) of
        -- If we went left at this node, then either:
        -- - We have already added the right value of the explosion to it's neighbour
        -- In this case, we just go back up, reconstituting the tree
        (LeftCrumb t, l', Nothing) -> rebuildTree crumbs (l', Nothing) $ Pair tree t
        -- - We haven't added the right value of the explosion to it's neighbour
        -- In this case, we go right instead and add the right value to the left-most node of that subtree
        (LeftCrumb t, l', Just rVal) -> rebuildTree crumbs (l', Nothing) (Pair tree (addLeft t rVal))
        -- (these cases work analagously)
        (RightCrumb t, Nothing, r') -> rebuildTree crumbs (Nothing, r') $ Pair t tree
        (RightCrumb t, Just lVal, r') -> rebuildTree crumbs (Nothing, r') (Pair (addRight t lVal) tree)

    -- addLeft adds rVal to the left-most node of the tree
    -- (addRight works analagously)
    addLeft (Regular x) rVal = Regular $ rVal + x
    addLeft (Pair l r) rVal = Pair (addLeft l rVal) r
    addRight (Regular x) lVal = Regular $ lVal + x
    addRight (Pair l r) lVal = Pair l (addRight r lVal)

-- split works similarly to explode
-- It returns a Just if a split happened, or a Nothing otherwise
split :: SnailTree Int -> Maybe (SnailTree Int)
split = split' []
  where
    split' crumbs (Pair l r) =
      split' (LeftCrumb r : crumbs) l
        <|> split' (RightCrumb l : crumbs) r
    split' crumbs (Regular x) =
      if x < 10
        then Nothing
        else
          Just $
            rebuildTree
              crumbs
              ( Pair
                  (Regular $ x `div` 2)
                  (Regular $ x `div` 2 + x `mod` 2)
              )
    rebuildTree [] tree = tree
    rebuildTree (c : crumbs) tree = case c of
      LeftCrumb r -> rebuildTree crumbs $ Pair tree r
      RightCrumb l -> rebuildTree crumbs $ Pair l tree

-- Simplify applies explode and split in the order specified until neither change the tree
simplify :: SnailTree Int -> SnailTree Int
simplify t = case (explode t, split t) of
  (Just t', _) -> simplify t'
  (Nothing, Just t') -> simplify t'
  (Nothing, Nothing) -> t

magnitude :: SnailTree Int -> Int
magnitude (Regular x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

partA :: Input -> OutputA
partA = magnitude . foldl1' (\m n -> simplify (Pair m n))

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let uniquePairs = filter (uncurry (/=)) $ (,) <$> input <*> input
   in maximum . fmap (\(m, n) -> magnitude . simplify $ Pair m n) $ uniquePairs
