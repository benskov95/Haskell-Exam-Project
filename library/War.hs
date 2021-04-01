module War where

import Card

startWar :: IO ()
startWar = do
  putStrLn "\nWelcome to 100% Bornholmsk Granit's War game!"
  deck <- shuffleDeck
  let (pDeck, eDeck) = splitDeck deck
  let (pCard, pDeck') = drawCards 1 pDeck
  let (eCard, eDeck') = drawCards 1 eDeck
  putStrLn $ "\nPlayer card: " ++ show pCard ++ ", value: " ++ cardValues pCard
  putStrLn $ "Enemy card: " ++ show eCard ", value: " ++ cardValues eCard

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
addToSideDeck [card] = sDeck ++ card
addToSideDeck [card:cards] = addToSideDeck cards sDeck

round :: (PlayerDecks, PlayerDecks) -> IO ()
round ((pDeck, pSideDeck), (eDeck, eSideDeck)) = do
    let (pCards, pDeck') = drawCards 1 pDeck
    let (eCards, eDeck') = drawCards 1 eDeck
    let pCard = head pCards
    let eCard = head eCards
    let roundResult = compareCardValues (pCard, eCard)

    if roundResult == Player then do
      let pSideDeck' = addToSideDeck [pCard, eCard] pSideDeck
      round ((pDeck', pSideDeck'), (eDeck', eSideDeck))
    else if roundResult == Enemy then do
      let eSideDeck' = addToSideDeck [pCard, eCard] eSideDeck
      round ((pDeck', pSideDeck), (eDeck', eSideDeck'))
    else
      war ((pDeck', pSideDeck), (eDeck', eSideDeck))

war :: (PlayerDecks, PlayerDecks) -> IO ()
war ((pDeck, pSideDeck), (eDeck, eSideDeck)) = do
    let (pCards, pDeck') = drawCards 4 pDeck
    let (eCards, eDeck') = drawCards 4 eDeck
    let pVisibleCard = last pCards
    let eVisibleCard = last eCards
    let warResult = compareCardValues (pVisibleCard, eVisibleCard)

    if roundResult == Player then do
      let pSideDeck' = addToSideDeck [pCards ++ eCards] pSideDeck
      round ((pDeck', pSideDeck'), (eDeck', eSideDeck))
    else if roundResult == Enemy then do
      let eSideDeck' = addToSideDeck [pCards ++ eCards] eSideDeck
      round ((pDeck', pSideDeck), (eDeck', eSideDeck'))
    else
      continuedWar ((pDeck', pSideDeck), (eDeck', eSideDeck)) (pCards, eCards)

continuedWar :: (PlayerDecks, PlayerDecks) -> (WonCards, WonCards) -> IO ()
continuedWar ((pDeck, pSideDeck), (eDeck, eSideDeck)) (pCards, eCards) =
    let (pCards', pDeck') = drawCards 4 pDeck
    let (eCards', eDeck') = drawCards 4 eDeck
    let pVisibleCard = last pCards'
    let eVisibleCard = last eCards'
    let allPCards = [pCards ++ pCards']
    let allECards = [eCards ++ eCards']
    let warResult = compareCardValues (pVisibleCard, eVisibleCard)

    if roundResult == Player then do
      let pSideDeck' = addToSideDeck [allPCards ++ allECards] pSideDeck
      round ((pDeck', pSideDeck'), (eDeck', eSideDeck))
    else if roundResult == Enemy then do
      let eSideDeck' = addToSideDeck [allPCards ++ allECards] eSideDeck
      round ((pDeck', pSideDeck), (eDeck', eSideDeck'))
    else
      continuedWar ((pDeck', pSideDeck), (eDeck', eSideDeck)) (allPCards, allECards)




