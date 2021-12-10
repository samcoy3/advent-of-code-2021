module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Functor (($>))
import Data.List ( foldl', sort )
import Data.Maybe ( mapMaybe )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( many1,
      sepBy1,
      endOfLine,
      Parser,
      choice,
      char,
      inClass,
      satisfy )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = line `sepBy1` endOfLine
  where
    line = mconcat <$> many1 chunk
    chunk =
      choice
        [ char '(' *> closeWith ')',
          char '[' *> closeWith ']',
          char '{' *> closeWith '}',
          char '<' *> closeWith '>'
        ]
    closeWith c =
      choice
        [ chunk >>= \chunk -> (chunk <>) <$> closeWith c,
          char c $> Fine,
          Corrupted <$> satisfy (inClass ")]}>"),
          return $ Incomplete [c]
        ]

------------ TYPES ------------
data ChunkType = Fine | Corrupted Char | Incomplete [Char]
  deriving (Eq, Show)

instance Semigroup ChunkType where
  Fine <> x = x
  Corrupted x <> y = Corrupted x
  Incomplete x <> Incomplete y = Incomplete (x <> y)
  Incomplete x <> _ = Incomplete x

instance Monoid ChunkType where
  mempty = Fine

type Input = [ChunkType]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
syntaxScore :: Char -> Int
syntaxScore ')' = 3
syntaxScore ']' = 57
syntaxScore '}' = 1197
syntaxScore '>' = 25137

isCorrupt :: ChunkType -> Bool
isCorrupt (Corrupted c) = True
isCorrupt _ = False

partA :: Input -> OutputA
partA = foldr errorScore 0
  where
    errorScore (Corrupted c) x = (+) (syntaxScore c) x
    errorScore _ x = x

------------ PART B ------------
autoCompleteScore :: String -> Int
autoCompleteScore = foldl' (\a x -> 5 * a + charScore x) 0
  where
    charScore ')' = 1
    charScore ']' = 2
    charScore '}' = 3
    charScore '>' = 4

getAutoComplete :: ChunkType -> Maybe [Char]
getAutoComplete (Incomplete cs) = Just cs
getAutoComplete _ = Nothing

partB :: Input -> OutputB
partB =
  (\scores -> scores !! ((length scores - 1) `div` 2))
    . sort
    . fmap autoCompleteScore
    . mapMaybe getAutoComplete
