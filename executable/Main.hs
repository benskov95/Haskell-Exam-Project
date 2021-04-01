import Blackjack
import War

main :: IO ()
main = do
  menu

menu :: IO ()
menu = do
    putStrLn "\n\nWhat would you like to play? Enter one of the following numbers to choose.\n"
    putStrLn "1: Blackjack\n2: War"
    input <- getLine
    if input == "1" then
      startBlackjack
    else if input == "2" then
      startWar
    else
      menu