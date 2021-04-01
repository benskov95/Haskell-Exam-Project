module Card where

import System.Random
import Data.List
import Text.Printf

data Card
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Show, Eq, Enum)

type Hand = [Card]
type Deck = [Card]

cardValues :: Card -> [Int]
cardValues Ace   = [1, 11]
cardValues Two   = [2]
cardValues Three = [3]
cardValues Four  = [4]
cardValues Five  = [5]
cardValues Six   = [6]
cardValues Seven = [7]
cardValues Eight = [8]
cardValues Nine  = [9]
cardValues _     = [10]

fullDeck :: Deck
fullDeck = concat $ replicate 4 [Ace .. King]

shuffleCards :: Deck -> Deck -> IO Deck
shuffleCards shuffled [] = return shuffled
shuffleCards shuffled unshuffled = do
  randomCardIndex <- randomRIO (0, (length unshuffled - 1))
  let randomCard = unshuffled !! randomCardIndex -- !! (indexing operator) means take element from the list at this index.
      unshuffledBefore = take randomCardIndex unshuffled
      unshuffledAfter = drop (randomCardIndex + 1) unshuffled

  shuffleCards (randomCard : shuffled) (unshuffledBefore ++ unshuffledAfter)

shuffleDeck :: IO Deck
shuffleDeck = shuffleCards [] fullDeck

dealCards :: Int -> Deck -> (Hand, Deck)
dealCards number deck = (take number deck, drop number deck)

handIsBlackjack :: Hand -> Bool
handIsBlackjack [card1, card2] =
  ((card1 == Ace) && elem card2 [Ten, Jack, Queen, King]) ||
  ((card2 == Ace) && elem card1 [Ten, Jack, Queen, King])
handIsBlackjack _ = False

handIsSoft :: Hand -> Bool
handIsSoft hand = Ace `elem` hand

possibleHandTotals :: Hand -> [Int] -> [Int] -- Value of an ace can vary depending on total value of hand at the time.
possibleHandTotals [] totals = sort $ nub totals
possibleHandTotals (card:cards) totals =
  possibleHandTotals cards newTotals
  where newTotals = [total + value | total <- totals, value <- cardValues card]

data Score = Value Int | Blackjack | Bust deriving (Show, Ord, Eq)

handScore :: Hand -> Score
handScore hand
  | null notBustTotals = Bust
  | handIsBlackjack hand = Blackjack
  | otherwise = Value (last notBustTotals)
  where notBustTotals = filter (<= 21) $ possibleHandTotals hand [0]

dealerNextMove :: Hand -> Deck -> (Hand, Deck)
dealerNextMove hand deck
  | score < Value 17 = hit hand deck
  | score == Value 17 = if handIsSoft hand then hit hand deck else (hand, deck)
  | otherwise = (hand, deck)
  where score = handScore hand

playerNextMove :: Hand -> Deck -> Int -> (Hand, Deck)
playerNextMove hand deck choice
  | choice == 1 = hit hand deck
  | choice == 2 = (hand, deck)
  | otherwise = (hand, deck)

hit :: Hand -> Deck -> (Hand, Deck)
hit hand deck = (newHand, newDeck) where
  newHand = hand ++ [head deck]
  newDeck = drop 1 deck








