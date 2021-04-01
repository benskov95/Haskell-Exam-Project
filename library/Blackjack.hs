module Blackjack where

import Card

import Text.Read
import System.Exit (exitSuccess)
import Data.List

-- GAME

startBlackjack :: IO ()
startBlackjack = do
    putStrLn "\n\nWelcome to 100% Bornholmsk Granit's Blackjack game!\n"
    deck <- shuffleDeck
    let (dHand, deck') = drawCards 2 deck
    let dVisibleCard = [head dHand]
    let (pHand, deck'') = drawCards 2 deck'
    putStrLn $ "Dealer hand: " ++ show dVisibleCard ++ ", " ++ show (handScore dVisibleCard)
    putStrLn $ "Your hand: " ++ show pHand ++ ", " ++ show (handScore pHand)
    makeMove pHand dHand deck''

currentHands :: Hand -> Hand -> Deck -> IO ()
currentHands pHand dHand deck = do
    putStrLn $ "\nDealer hand: " ++ show dHand ++ ", " ++ show (handScore dHand)
    putStrLn $ "Your hand: " ++ show pHand ++ ", " ++ show (handScore pHand)
    outcome pHand dHand
    makeMove pHand dHand deck

makeMove :: Hand -> Hand -> Deck -> IO ()
makeMove pHand dHand deck = do
    if handScore pHand == Value 21 then do
      let (dNewHand, deck') = dealerNextMove dHand deck
      currentHands pHand dNewHand deck'
    else do
      putStrLn "\nEnter 1 to hit or 2 to stand\n"
      input <- getLine
      let mNum = readMaybe input :: Maybe Int
      case mNum of
        Just number ->
          if number == 1 then do
            let (pNewHand, deck') = playerNextMove pHand deck number
            currentHands pNewHand [head dHand] deck'
            else if number == 2 then do
              let (dNewHand, deck') = dealerNextMove dHand deck
              currentHands pHand dNewHand deck'
          else
            makeMove pHand dHand deck
        Nothing -> makeMove pHand dHand deck

outcome :: Hand -> Hand -> IO ()
outcome pHand dHand
  | handScore pHand == Bust = do
     putStrLn $ "\nPlayer bust. Dealer wins! " ++ askToStartAgain
     playAgain
  | handScore dHand == Bust = do
      putStrLn $ "\nDealer bust. You win! " ++ askToStartAgain
      playAgain
  | handScore pHand == Blackjack = do
      putStrLn $ "\nYou got Blackjack! " ++ askToStartAgain
      playAgain
  | handScore dHand == Blackjack = do
     putStrLn $ "\nDealer got Blackjack! " ++ askToStartAgain
     playAgain
  | handScore pHand < handScore dHand && dealerStanding (handScore dHand) = do
     putStrLn $ "\nYou lose! " ++ askToStartAgain
     playAgain
  | handScore pHand > handScore dHand && dealerStanding (handScore dHand) = do
    putStrLn $ "\nYou win! " ++ askToStartAgain
    playAgain
  | handScore pHand == handScore dHand && dealerStanding (handScore dHand) = do
    putStrLn $ "\nPush (tie)! " ++ askToStartAgain
    playAgain
  | otherwise = return ()

dealerStanding :: Score -> Bool
dealerStanding score =
  if score >= Value 17 && score <= Value 21 then
    True
  else
    False

playAgain :: IO ()
playAgain = do
   input <- getLine
   if input == "1" then
     startBlackjack
   else
     exitSuccess

askToStartAgain :: String
askToStartAgain = "\nWould you like to play again? Press 1 for yes, anything else for no"

-- Functions for Blackjack

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

data Score = Value Int | Blackjack | Bust deriving (Show, Ord, Eq)

handIsBlackjack :: Hand -> Bool
handIsBlackjack [card1, card2] =
  ((card1 == Ace) && elem card2 [Ten, Jack, Queen, King]) ||
  ((card2 == Ace) && elem card1 [Ten, Jack, Queen, King])
handIsBlackjack _ = False

handIsSoft :: Hand -> Bool
handIsSoft hand = Ace `elem` hand

handScore :: Hand -> Score
handScore hand
  | null notBustTotals = Bust
  | handIsBlackjack hand = Blackjack
  | otherwise = Value (last notBustTotals)
  where notBustTotals = filter (<= 21) $ possibleHandTotals hand [0]

dealerNextMove :: Hand -> Deck -> (Hand, Deck)
dealerNextMove hand deck
  | score < Value 17 = do
    let (dNewHand, deck') = hit hand deck
    dealerNextMove dNewHand deck'
  | score == Value 17 = if handIsSoft hand then hit hand deck else (hand, deck)
  | otherwise = (hand, deck)
  where score = handScore hand

playerNextMove :: Hand -> Deck -> Int -> (Hand, Deck)
playerNextMove hand deck choice'
  | choice' == 1 = hit hand deck
  | choice' == 2 = (hand, deck)
  | otherwise = (hand, deck)

hit :: Hand -> Deck -> (Hand, Deck)
hit hand deck = (newHand, newDeck) where
  newHand = hand ++ [head deck]
  newDeck = drop 1 deck

possibleHandTotals :: Hand -> [Int] -> [Int] -- Value of an ace can vary depending on total value of hand at the time.
possibleHandTotals [] totals = sort $ nub totals
possibleHandTotals (card:cards) totals =
  possibleHandTotals cards newTotals
  where newTotals = [total + value | total <- totals, value <- cardValues card]

