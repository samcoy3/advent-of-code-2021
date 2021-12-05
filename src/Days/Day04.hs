module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>), many)
import Data.List ( find, scanl', transpose )
import Data.Maybe ( fromJust )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser,
      count,
      sepBy,
      sepBy1,
      decimal,
      skipSpace,
      char,
      endOfLine )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> (calledNumbers <* count 2 endOfLine) <*> bingoCards
  where
    calledNumbers = decimal `sepBy` char ','
    bingoCards =
      count 5 (count 5 $ skipSpace *> ((,False) <$> decimal))
        `sepBy1` count 2 endOfLine

------------ TYPES ------------
-- Representing a bingo card as a 2D list of pairs of ints and bools.
-- The int represents the number in the square; the bool indicates if it's been called.
type BingoCard = [[(Int, Bool)]]

type Input = ([Int], [BingoCard])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Given a bingo card, checks whether it is complete.
bingoCardComplete :: BingoCard -> Bool
bingoCardComplete card =
  any (and . fmap snd) card
    || any (and . fmap snd) (transpose card)

-- Given a number called out and a bingo card, fills in that number on the card.
callNumberOnCard :: Int -> BingoCard -> BingoCard
callNumberOnCard number =
  fmap
    ( fmap
        (\square -> if square == (number, False) then (number, True) else square)
    )

-- Plays a bingo game.
-- Input is the sequence of numbers to be called and a list of bingo cards.
-- Returns a list of turns of bingo. Each card is removed the turn after it wins.
bingoGame :: Input -> [(Int, [BingoCard])]
bingoGame (calledNumbers, cards) =
  zip calledNumbers . tail $
    scanl' (\cards number -> callNumberOnCard number <$> filter (not . bingoCardComplete) cards) cards calledNumbers

-- Scores a bingo card according to the definition.
cardScore :: Int -> BingoCard -> Int
cardScore numberCalled card =
  (*) numberCalled $
    sum . fmap fst . filter (not . snd) . concat $ card

-- Plays a game of bingo, stopping the game and scoring any winning cards once a condition is met.
playGameUntilCardsComplete :: ((BingoCard -> Bool) -> [BingoCard] -> Bool) -> Input -> Int
playGameUntilCardsComplete condition input = sum . fmap (cardScore lastNumber) . filter bingoCardComplete $ cards
  where
    (lastNumber, cards) = fromJust . find (condition bingoCardComplete . snd) . bingoGame $ input

partA :: Input -> OutputA
partA = playGameUntilCardsComplete any

------------ PART B ------------
partB :: Input -> OutputB
partB = playGameUntilCardsComplete all
