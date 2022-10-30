module Main where

import Game
import System.Console.ANSI
import System.IO

main :: IO ()
main = do
        clearScreen
        putStrLn "----------------------------"
        putStrLn "        Welcome to"
        putStrLn "     Eat the Fruit Game"
        putStrLn "----------------------------"
        putStrLn "Select menu below :"
        putStrLn "[1] Play the game!"
        putStrLn "[2] Show Score List"
        putStrLn "[3] Game Rules"
        putStrLn "----------------------------"
        putStrLn "[x] Exit"
        putStrLn "----------------------------"
        putStrLn "Input your Choice : "

        chosenMenu <- getLine
        case chosenMenu of
                "1" -> do
                        gameMain
                        main
                "2" -> scoreList
                "3" -> gameRules
                "x" -> do
                        clearScreen
                        putStrLn "Thank you, See you later!"
                _ -> main

scoreList :: IO ()
scoreList = do
            clearScreen
            putStrLn "----------------------------------------------------------------"
            putStrLn "                        Score History"
            putStrLn "----------------------------------------------------------------"
            scoreList <- readFile "data/scorelist.txt"
            if null scoreList
                then putStrLn "There is no history yet!"
                else putStr scoreList 
            putStrLn "----------------------------------------------------------------"

            putStrLn "Input any Key to go back : "
            back <- getLine
            main

gameRules :: IO ()
gameRules = do
            clearScreen
            putStrLn "-------------------------------------------------------"
            putStrLn "                     Game Rules"
            putStrLn "-------------------------------------------------------"
            putStrLn "Welcome to \"Eat the Fruit\" Game!\n"
            putStrLn "I.   How to play"
            putStrLn "     Always try to eat the fruit, and don't let your"
            putStrLn "     character hit the wall.\n"
            putStrLn "II.  Score"
            putStrLn "     Your score will continue to increase every time"
            putStrLn "     you eat the fruit, there are two types of fruit:"
            putStrLn "     - Regular Fruit (o) : 1 score"
            putStrLn "     - Bonus Fruit   (O) : 2 score\n"
            putStrLn "III. Difficulty"
            putStrLn "     The difficulty level will continue to increase"
            putStrLn "     every ten fruits you eat. Each level of difficulty"
            putStrLn "     increases means the speed of your character will"
            putStrLn "     also increase."
            putStrLn "-------------------------------------------------------"

            putStrLn "Input any Key to go back : "
            back <- getLine
            main