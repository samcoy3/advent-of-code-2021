{-# LANGUAGE TupleSections #-}

module Days.Day16 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ( Alternative((<|>), empty) )
import Data.Tuple (swap)
import Data.List ( foldl' )
import Data.Maybe ( fromJust, isNothing )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text (Parser, many1, digit, letter)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = concatMap decToBin . fmap hexDigit <$> many1 (letter <|> digit)
  where
    hexDigit 'A' = 10
    hexDigit 'B' = 11
    hexDigit 'C' = 12
    hexDigit 'D' = 13
    hexDigit 'E' = 14
    hexDigit 'F' = 15
    hexDigit x = read (pure x)
    -- Converts an int to a list of bools (and pads to at least length 4)
    decToBin = (\bs -> replicate (4 - length bs) False ++ bs) . decToBin'
    decToBin' 0 = [False]
    decToBin' 1 = [True]
    decToBin' x = decToBin' (x `div` 2) ++ [x `mod` 2 == 1]

------------ TYPES ------------
type Input = [Bool]

data Packet = Packet Int PacketType deriving (Eq, Show)

-- The Int attached to Literal is the literal value
-- The Int attached to SubPackets is the Type ID (needed to determine opcode)
data PacketType = Literal Int | SubPackets Int [Packet] deriving (Eq, Show)

type OutputA = Int

type OutputB = Int

-- A parser type that consumes a list of bits
-- I am relatively certain that the alternative instance (or perhaps the applicative instance) is faulty, as `count` and `many` infinite-looped
-- TODO: Fix these instances
newtype PacketParser a = PacketParser {parse :: [Bool] -> Maybe ([Bool], a)}
  deriving (Functor)

instance Applicative PacketParser where
  pure x = PacketParser (Just . (,x))
  (PacketParser x) <*> (PacketParser y) = PacketParser $
    \bs -> do
      (bs', x') <- x bs
      (bs'', y') <- y bs
      return (bs'', x' y')

instance Monad PacketParser where
  (PacketParser x) >>= f = PacketParser $
    \bs -> do
      (bs', x') <- x bs
      let (PacketParser y) = f x'
      (bs'', y') <- y bs'
      return (bs'', y')

instance Alternative PacketParser where
  empty = PacketParser $ const Nothing
  (PacketParser x) <|> (PacketParser y) = PacketParser $
    \bs -> if isNothing (x bs) then y bs else x bs

------------ PART A ------------
-- Converts a list of bools to an integer (MSB at the head of the list)
binToDec :: [Bool] -> Int
binToDec = foldl' (\a x -> a * 2 + fromEnum x) 0

-- A parser that takes `len` bits from our list of bits
takeBits :: Int -> PacketParser [Bool]
takeBits len = PacketParser $ \bs ->
  if length bs < len
    then Nothing
    else Just . swap $ splitAt len bs

-- A parser that reads an integer composed of a certain number of bits
parseInt :: Int -> PacketParser Int
parseInt len = binToDec <$> takeBits len

-- Parsers a literal packet
parseLiteral :: PacketParser Int
parseLiteral = binToDec <$> getBitsForLiteral
  where
    getBitsForLiteral = do
      group <- takeBits 5
      if head group
        then (tail group ++) <$> getBitsForLiteral
        else return $ tail group

-- Parses a packet when the quantity of sub-packets is given by number
parseNumberOfSubPackets :: PacketParser [Packet]
parseNumberOfSubPackets = do
  numberToTake <- parseInt 11
  let parseSubPackets count =
        if count == 0
          then return []
          else do
            packet <- parsePacket
            (packet :) <$> parseSubPackets (count - 1)
  parseSubPackets numberToTake

-- Parses a packet when the quantity of sub-packets is given by total length
parseLengthOfSubPackets :: PacketParser [Packet]
parseLengthOfSubPackets = do
  subpackets <- parseInt 15 >>= takeBits
  let parseSubPackets bs = case parse parsePacket bs of
        Nothing -> []
        Just (bs', packet) -> packet : parseSubPackets bs'
  return $ parseSubPackets subpackets

-- Parses one packet (recursively)
parsePacket :: PacketParser Packet
parsePacket = do
  version <- parseInt 3
  typeID <- parseInt 3
  packetType <-
    if typeID == 4
      then Literal <$> parseLiteral
      else do
        lengthTypeID <- head <$> takeBits 1
        SubPackets typeID
          <$> if lengthTypeID
            then parseNumberOfSubPackets
            else parseLengthOfSubPackets
  return $ Packet version packetType

-- Sums the versions of packets, recursively
sumPacketVersions :: Packet -> Int
sumPacketVersions (Packet v (Literal _)) = v
sumPacketVersions (Packet v (SubPackets _ ps)) = v + sum (sumPacketVersions <$> ps)

partA :: Input -> OutputA
partA = sumPacketVersions . snd . fromJust . parse parsePacket

------------ PART B ------------
-- Uses the type ID to recursively find the value of the packet
findValueOfPacket :: Packet -> Int
findValueOfPacket (Packet _ (Literal x)) = x
findValueOfPacket (Packet _ (SubPackets f xs)) = op f $ findValueOfPacket <$> xs
  where
    op 0 = sum
    op 1 = product
    op 2 = minimum
    op 3 = maximum
    op 5 = \(x : y : _) -> fromEnum (x > y)
    op 6 = \(x : y : _) -> fromEnum (x < y)
    op 7 = \(x : y : _) -> fromEnum (x == y)

partB :: Input -> OutputB
partB = findValueOfPacket . snd . fromJust . parse parsePacket
