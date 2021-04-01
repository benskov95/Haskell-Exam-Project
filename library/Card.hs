module Card where

import System.Random

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

splitDeck :: Deck -> (Deck, Deck)
splitDeck deck = do
  let firstDeck = take ((length deck) `div` 2) deck
  let secondDeck = drop ((length deck) `div` 2) deck
  (firstDeck, secondDeck)

drawCards :: Int -> Deck -> (Hand, Deck)
drawCards number deck = (take number deck, drop number deck)


