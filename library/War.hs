module War where

import Card

startWar :: IO ()
startWar = do
  putStrLn "\nWelcome to 100% Bornholmsk Granit's War game!"
  deck <- shuffleDeck
  let (pDeck, eDeck) = splitDeck deck
  let (pSideDeck, eSideDeck) = emptyDecks
  startRound ((pDeck, pSideDeck), (eDeck, eSideDeck))

cardValues :: Card -> Int
cardValues Two   = 2
cardValues Three = 3
cardValues Four  = 4
cardValues Five  = 5
cardValues Six   = 6
cardValues Seven = 7
cardValues Eight = 8
cardValues Nine  = 9
cardValues Ten   = 10
cardValues Jack  = 11
cardValues Queen = 12
cardValues King  = 13
cardValues Ace   = 14

data Winner = Player | Enemy | War deriving (Show)

type SideDeck = Deck
type PlayerDecks = (Deck, SideDeck)
type WonCards = [Card]

compareCardValues :: (Card, Card) -> Winner
compareCardValues (pCard, eCard) =
    if cardValues pCard > cardValues eCard then
      Player
    else if cardValues pCard < cardValues eCard then
      Enemy
    else
      War

addToSideDeck :: WonCards -> SideDeck -> SideDeck
addToSideDeck [] sDeck = sDeck
addToSideDeck [card] sDeck = sDeck ++ [card]
addToSideDeck [card:cards] sDeck = addToSideDeck cards sDeck

getStatus :: (PlayerDecks, PlayerDecks) -> (Card, Card) -> IO ()
getStatus ((pDeck, pSideDeck), (eDeck, eSideDeck)) (pCard, eCard) = do
    putStrLn $ ("\nPlayer card: " ++ show pCard ++
                ", value: " ++ show (cardValues pCard) ++
                "\nCards in deck: " ++ show (length pDeck) ++
                "\nCards in side deck: " ++ show (length pSideDeck)
                )

    putStrLn $ ("\nEnemy card: " ++ show eCard ++
                ", value: " ++ show (cardValues eCard) ++
                "\nCards in deck: " ++ show (length eDeck) ++
                "\nCards in side deck: " ++ show (length eSideDeck)
                )

startRound :: (PlayerDecks, PlayerDecks) -> IO ()
startRound ((pDeck, pSideDeck), (eDeck, eSideDeck)) = do
    let (pCards, pDeck') = drawCards 1 pDeck
    let (eCards, eDeck') = drawCards 1 eDeck
    let pCard = head pCards
    let eCard = head eCards
    let roundResult = compareCardValues (pCard, eCard)

    getStatus ((pDeck', pSideDeck), (eDeck', eSideDeck)) (pCard, eCard)

    if roundResult == Player then do
      putStrLn "\nYou won this round!"
      let pSideDeck' = addToSideDeck [pCard, eCard] pSideDeck
      startRound ((pDeck', pSideDeck'), (eDeck', eSideDeck))
    else if roundResult == Enemy then do
      putStrLn "\nYou lost this round :("
      let eSideDeck' = addToSideDeck [pCard, eCard] eSideDeck
      startRound ((pDeck', pSideDeck), (eDeck', eSideDeck'))
    else
      putStrLn "\nWAR!"
      war ((pDeck', pSideDeck), (eDeck', eSideDeck))

war :: (PlayerDecks, PlayerDecks) -> IO ()
war ((pDeck, pSideDeck), (eDeck, eSideDeck)) = do
    let (pCards, pDeck') = drawCards 4 pDeck
    let (eCards, eDeck') = drawCards 4 eDeck
    let pVisibleCard = last pCards
    let eVisibleCard = last eCards
    let warResult = compareCardValues (pVisibleCard, eVisibleCard)

    getStatus ((pDeck', pSideDeck), (eDeck', eSideDeck)) (pVisibleCard, eVisibleCard)

    if warResult == Player then do
      putStrLn "\nYou won the war!"
      let pSideDeck' = addToSideDeck [pCards ++ eCards] pSideDeck
      startRound ((pDeck', pSideDeck'), (eDeck', eSideDeck))
    else if warResult == Enemy then do
      putStrLn "\nYou lost the war :("
      let eSideDeck' = addToSideDeck [pCards ++ eCards] eSideDeck
      startRound ((pDeck', pSideDeck), (eDeck', eSideDeck'))
    else
      putStrLn "\nThe war continues!"
      continuedWar ((pDeck', pSideDeck), (eDeck', eSideDeck)) (pCards, eCards)

continuedWar :: (PlayerDecks, PlayerDecks) -> (WonCards, WonCards) -> IO ()
continuedWar ((pDeck, pSideDeck), (eDeck, eSideDeck)) (pCards, eCards) = do
    let (pCards', pDeck') = drawCards 4 pDeck
    let (eCards', eDeck') = drawCards 4 eDeck
    let pVisibleCard = last pCards'
    let eVisibleCard = last eCards'
    let allPCards = [pCards ++ pCards']
    let allECards = [eCards ++ eCards']
    let warResult = compareCardValues (pVisibleCard, eVisibleCard)

    getStatus ((pDeck', pSideDeck), (eDeck', eSideDeck)) (pVisibleCard, eVisibleCard)

    if warResult == Player then do
      putStrLn "\nYou won the war at last!"
      let pSideDeck' = addToSideDeck [allPCards ++ allECards] pSideDeck
      startRound ((pDeck', pSideDeck'), (eDeck', eSideDeck))
    else if warResult == Enemy then do
      putStrLn "\nYou lost the war... what a shame."
      let eSideDeck' = addToSideDeck [allPCards ++ allECards] eSideDeck
      startRound ((pDeck', pSideDeck), (eDeck', eSideDeck'))
    else
      putStrLn "\nThe war just keeps on going..."
      continuedWar ((pDeck', pSideDeck), (eDeck', eSideDeck)) (allPCards, allECards)




