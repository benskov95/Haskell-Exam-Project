module War where

import System.Exit (exitSuccess)

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

data Winner = Player | Enemy | War deriving (Show, Eq)

type SideDeck = Deck
type PlayerDecks = (Deck, SideDeck)
type LastRoundCards = [Card]
type WonCards = [Card]

startRound :: (PlayerDecks, PlayerDecks) -> IO ()
startRound ((pDeck, pSideDeck), (eDeck, eSideDeck)) = do
  if (length pDeck) == 0 || (length eDeck) == 0 then do
    nextGame ((pDeck, pSideDeck), (eDeck, eSideDeck))
  else do
    putStrLn "\nPress any key to start the next round."
    _ <- getLine -- For pacing. The program just runs until the end immediately otherwise.

    let (pCards, pDeck') = drawCards 1 pDeck
    let (eCards, eDeck') = drawCards 1 eDeck
    let cardsFromRound = pCards ++  eCards
    let roundResult = compareCardValues (head pCards, head eCards)

    getStatus ((pDeck', pSideDeck), (eDeck', eSideDeck)) (head pCards, head eCards)

    if roundResult == Player then do
      putStrLn "\nYou won this round!"
      let pSideDeck' = addToSideDeck cardsFromRound pSideDeck
      startRound ((pDeck', pSideDeck'), (eDeck', eSideDeck))
    else if roundResult == Enemy then do
      putStrLn "\nYou lost this round :("
      let eSideDeck' = addToSideDeck cardsFromRound eSideDeck
      startRound ((pDeck', pSideDeck), (eDeck', eSideDeck'))
    else do
      putStrLn "\nWAR!"
      war ((pDeck', pSideDeck), (eDeck', eSideDeck)) cardsFromRound

-- Need cards from the round the war started in the total, otherwise cards go missing.
war :: (PlayerDecks, PlayerDecks) -> LastRoundCards -> IO ()
war ((pDeck, pSideDeck), (eDeck, eSideDeck)) lastRoundCards = do
  let ((pDeck', pSideDeck'), (eDeck', eSideDeck')) = checkIfUseSideDeck ((pDeck, pSideDeck), (eDeck, eSideDeck))
  let (pCards, pDeck'') = drawCards 4 pDeck'
  let (eCards, eDeck'') = drawCards 4 eDeck'

  let pVisibleCard = last pCards
  let eVisibleCard = last eCards
  let warResult = compareCardValues (pVisibleCard, eVisibleCard)
  let allCardsOnTable = (pCards ++ eCards ++ lastRoundCards)

  warOutcome ((pDeck'', pSideDeck'), (eDeck'', eSideDeck'))
  getStatus ((pDeck'', pSideDeck'), (eDeck'', eSideDeck')) (pVisibleCard, eVisibleCard)

  if warResult == Player then do
    putStrLn "\nYou won the war!"
    let pSideDeck'' = addToSideDeck allCardsOnTable pSideDeck
    startRound ((pDeck'', pSideDeck''), (eDeck'', eSideDeck'))
  else if warResult == Enemy then do
    putStrLn "\nYou lost the war :("
    let eSideDeck'' = addToSideDeck allCardsOnTable eSideDeck
    startRound ((pDeck'', pSideDeck'), (eDeck'', eSideDeck''))
  else do
    putStrLn "\nThe war continues!"
    continuedWar ((pDeck'', pSideDeck'), (eDeck'', eSideDeck')) allCardsOnTable

continuedWar :: (PlayerDecks, PlayerDecks) -> WonCards -> IO ()
continuedWar ((pDeck, pSideDeck), (eDeck, eSideDeck)) cardsFromWar = do
  let ((pDeck', pSideDeck'), (eDeck', eSideDeck')) = checkIfUseSideDeck ((pDeck, pSideDeck), (eDeck, eSideDeck))
  let (pCards', pDeck'') = drawCards 4 pDeck'
  let (eCards', eDeck'') = drawCards 4 eDeck'

  let pVisibleCard = last pCards'
  let eVisibleCard = last eCards'
  let currentCardsOnTable = (cardsFromWar ++ pCards' ++ eCards')
  let warResult = compareCardValues (pVisibleCard, eVisibleCard)

  warOutcome ((pDeck'', pSideDeck'), (eDeck'', eSideDeck'))
  getStatus ((pDeck'', pSideDeck'), (eDeck'', eSideDeck')) (pVisibleCard, eVisibleCard)

  if warResult == Player then do
    putStrLn "\nYou won the war at last!"
    let pSideDeck'' = addToSideDeck currentCardsOnTable pSideDeck
    startRound ((pDeck'', pSideDeck''), (eDeck'', eSideDeck'))
  else if warResult == Enemy then do
    putStrLn "\nYou lost the war... what a shame."
    let eSideDeck'' = addToSideDeck currentCardsOnTable eSideDeck
    startRound ((pDeck'', pSideDeck'), (eDeck'', eSideDeck''))
  else do
    putStrLn "\nThe war just keeps on going..."
    continuedWar ((pDeck'', pSideDeck'), (eDeck'', eSideDeck')) currentCardsOnTable

compareCardValues :: (Card, Card) -> Winner
compareCardValues (pCard, eCard) =
    if cardValues pCard > cardValues eCard then
      Player
    else if cardValues pCard < cardValues eCard then
      Enemy
    else
      War

addToSideDeck :: WonCards -> SideDeck -> SideDeck
addToSideDeck cards sDeck = newSDeck where
  newSDeck = sDeck ++ cards

getStatus :: (PlayerDecks, PlayerDecks) -> (Card, Card) -> IO ()
getStatus ((pDeck, pSideDeck), (eDeck, eSideDeck)) (pCard, eCard) = do
    putStrLn $ ("\nPlayer (you)" ++
                "\nCard: " ++ show pCard ++
                ", value: " ++ show (cardValues pCard) ++
                "\nCards in deck: " ++ show (length pDeck) ++
                "\nCards in side deck: " ++ show (length pSideDeck)
                )

    putStrLn $ ("\nEnemy " ++
                "\nCard: " ++ show eCard ++
                ", value: " ++ show (cardValues eCard) ++
                "\nCards in deck: " ++ show (length eDeck) ++
                "\nCards in side deck: " ++ show (length eSideDeck)
                )

nextGame :: (PlayerDecks, PlayerDecks) -> IO ()
nextGame ((pDeck, pSideDeck), (eDeck, eSideDeck))
  | length (pDeck ++ pSideDeck) == 0 = do
    putStrLn "\nYou lost the game! Better luck next time."
    exitSuccess
  | length (eDeck ++ eSideDeck) == 0 = do
    putStrLn "\nCongratulations, you won the game!"
    exitSuccess
  | otherwise = do
    let (pSideDeckNew, eSideDeckNew) = emptyDecks
    let pCombinedDeck = pDeck ++ pSideDeck
    let eCombinedDeck = eDeck ++ eSideDeck
    startRound ((pCombinedDeck, pSideDeckNew), (eCombinedDeck, eSideDeckNew))

warOutcome :: (PlayerDecks, PlayerDecks) -> IO ()
warOutcome ((pDeck, pSideDeck), (eDeck, eSideDeck))
  | length (pDeck ++ pSideDeck) == 0 = do
      putStrLn "\nYou ran out of cards during the war and lost. Better luck next time."
      exitSuccess
  | length (eDeck ++ eSideDeck) == 0 = do
      putStrLn "\nYour opponent ran out of cards during the war, so you won the game. Congratulations!"
      exitSuccess
  | (((length (pDeck ++ pSideDeck)) == 0) && ((length (eDeck ++ eSideDeck) == 0))) = do
      putStrLn "\nTIE! Both you and your opponenent ran out of cards during the war."
      exitSuccess
  | otherwise = return ()

checkIfUseSideDeck :: (PlayerDecks, PlayerDecks) -> (PlayerDecks, PlayerDecks)
checkIfUseSideDeck ((pDeck, pSideDeck), (eDeck, eSideDeck)) =
  if (length pDeck) < 4 && (length pSideDeck) >= 4 then
    (((pDeck ++ pSideDeck), []), (eDeck, eSideDeck))
  else if (length eDeck) < 4 && (length eSideDeck) >= 4 then
    ((pDeck, pSideDeck), ((eDeck ++ eSideDeck), []))
  else
    ((pDeck, pSideDeck), (eDeck, eSideDeck))