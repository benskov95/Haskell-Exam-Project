{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Card
import Text.Read

main :: IO ()
main = do
  startGame

startGame :: IO ()
startGame = do
    putStrLn "\nHey, let's play some blackjack\n"
    deck <- shuffleDeck
    let (dHand, deck') = dealCards 2 deck
    let dVisibleCard = [head dHand]
    let (pHand, deck'') = dealCards 2 deck'
    putStrLn $ "Dealer hand: " ++ show dVisibleCard ++ ", " ++ show (handScore dVisibleCard)
    putStrLn $ "Your hand: " ++ show pHand ++ ", " ++ show (handScore pHand)
    makeMove pHand dHand deck''

currentHands :: Hand -> Hand -> Deck -> IO ()
currentHands pHand dHand deck = do
    putStrLn $ "Dealer hand: " ++ show dHand ++ ", " ++ show (handScore dHand)
    putStrLn $ "Your hand: " ++ show pHand ++ ", " ++ show (handScore pHand)
    makeMove pHand dHand deck

makeMove :: Hand -> Hand -> Deck -> IO ()
makeMove pHand dHand deck = do
    putStrLn "\nEnter 1 to hit or 2 to stand"
    input <- getLine
    let mNum = readMaybe input :: Maybe Int
    case mNum of
      Just number ->
        if number == 1 || number == 2 then do
          let (pNewHand, deck') = playerNextMove pHand deck number
          let (dNewHand, deck'') = dealerNextMove dHand deck'
          currentHands pNewHand dNewHand deck''
        else
          makeMove pHand dHand deck
      Nothing -> makeMove pHand dHand deck


