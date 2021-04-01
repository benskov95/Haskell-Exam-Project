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

possibleHandTotals :: Hand -> [Int] -> [Int] -- Value of an ace can vary depending on total value of hand at the time.
possibleHandTotals [] totals = sort $ nub totals
possibleHandTotals (card:cards) totals =
  possibleHandTotals cards newTotals
  where newTotals = [total + value | total <- totals, value <- cardValues card]




