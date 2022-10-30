module Game where

import System.IO
import Control.Concurrent
import Control.Concurrent.Async
import System.Timeout
import System.Console.ANSI
import System.Random
import Control.Applicative
import Control.Monad.Loops
import Data.Time.Clock
 
-- TYPES
type Vector = (Int, Int)
 
data State = State { position :: Vector, 
                     move :: Maybe Vector, -- Player's direction
                     env :: Vector, -- env = (width, height)
                     fruit :: (Vector, StdGen),
                     score :: Int,
                     difficulty :: Int, -- used for increase the player's speed difficulty
                     bonusFruit :: Bool -- score of bonus fruit is 2
                    } 
  deriving (Show)
 
-- Draw
drawState :: State -> String
drawState state@(State { env = (_, height)})
  = unlines $ reverse $ map (\row -> drawRow state row) [-1..height+1]
 
drawRow :: State -> Int -> String
drawRow state@(State { env = (width, _)}) row 
  = map (\col -> charAt state (col, row)) [-1..width+1]
 
charAt :: State -> Vector -> Char
charAt state@(State{ position = (posX, posY), 
                     env = (width, height), 
                     fruit = ((fruitX,fruitY), _),
                     bonusFruit = bonusFruit}) (x, y)
                        | (posX, posY) == (x, y) = '@'
                        | (fruitX, fruitY) == (x,y) && bonusFruit = 'O'
                        | (fruitX, fruitY) == (x,y) && (not bonusFruit) = 'o'
                        | y < 0 || y > height = '-'
                        | x < 0 || x > width = '|'
                        | otherwise = ' '

generateWorld :: State -> IO ()
generateWorld state = do
                      if gameOver state
                        then saveData (show (score state))
                          else putStrLn ("Score : " ++ show (score state))
                      
                      putStr (drawState state)

saveData :: String -> IO ()
saveData score = do
                  hSetEcho stdin True
                  hSetBuffering stdin LineBuffering
                  hSetBuffering stdout NoBuffering
                  putStrLn "----------------------------"
                  putStrLn "         Game Over"
                  putStrLn "----------------------------"
                  putStrLn ("Your score is : " ++ score)
                  putStrLn "----------------------------"
                  putStr "What is your name? "
                  name <- getLine

                  time <- getCurrentTime
                  let currentTime = show time

                  putStrLn "----------------------------"
                  putStrLn "Saving your score..."
                  threadDelay 1000000
                  let file_path = "data/scorelist.txt"
                  let content = "Score: " ++ score ++ ", Name: " ++ name ++ ", Time: " ++ currentTime ++ "\n"
                  appendFile file_path content

                  hSetBuffering stdout LineBuffering -- set back to default

-- Logic
gameMain :: IO State       
gameMain = clearScreen
        >> initialState
        >>= (iterateUntilM gameOver movePlayer)

initialState :: IO State
initialState = getStdGen 
                >>= \stdGen -> return State {
                 position = (5,6),
                 move = Just (1,0),
                 env = (20,10),
                 fruit = randomVector (20,10) stdGen,
                 score = 0,
                 difficulty = 1, --used as divisor in actionLength
                 bonusFruit = False
              }

gameOver :: State -> Bool -- Check if the player's pos still in safe area
gameOver state@(State {position = (posX,posY), env = (width, height)})
    | posY < 0 = True
    | posX < 0 = True
    | posY > height = True
    | posX > width = True
    | otherwise = False

movePlayer :: State -> IO State -- Move the player's character
movePlayer state = clearScreen 
                   >> action actionLength getInput 
                   >>= \ inputMove -> displayState $ updateState state (getVector inputMove)
                   
                   where actionLength = oneSecond `div` (3 + difficulty state) 
                   -- Divisor value will continue to increase according to the amoung of difficulty

action :: Int -> IO a -> IO (Maybe a)
action timeLength input = concurrently (timeout timeLength input) (threadDelay timeLength) 
                          >>= \ (result, _) -> return result

oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int) -- microsecond format

getInput :: IO Char -- Take single char of user input
getInput = hSetEcho stdin False 
    >> hSetBuffering stdin NoBuffering
    >> getChar

getVector :: Maybe Char -> Maybe Vector
getVector (Just 'w') = Just (0,1)
getVector (Just 'a') = Just (-1,0)
getVector (Just 's') = Just (0,-1)
getVector (Just 'd') = Just (1,0)
getVector _ = Nothing

displayState :: State -> IO State -- generate the state
displayState state = generateWorld state >> return state

updateState :: State -> Maybe Vector -> State
updateState state inputMove = updateDifficulty $ updateFruit $ updatePos $ updateMove state inputMove

updateMove :: State -> Maybe Vector -> State
updateMove state inputMove@(Just inputVector) = state {move = inputMove }
updateMove state _ = state

updatePos :: State -> State -- update the player's position
updatePos state@(State { position = pos, move = Just vector})
    = state { position = pos `addVector` vector}

addVector :: Vector -> Vector -> Vector
addVector (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

updateFruit :: State -> State
updateFruit state@(State {score = score})
  | fruitEaten state = state {fruit = newFruit state, score = score + 1} -- Add 1 score for each food eaten
  | otherwise = state

fruitEaten :: State -> Bool
fruitEaten state@(State {position = pos, fruit = fruit})
    = fruit `posEquals` pos

posEquals :: (Vector, StdGen) -> Vector -> Bool
posEquals (position, _) vector = position == vector

newFruit :: State -> (Vector, StdGen)
newFruit state@(State {env = env, fruit = (_, stdGen)})
    = randomVector env stdGen

updateDifficulty :: State -> State
updateDifficulty state@(State {score = score, difficulty = difficulty})
  -- Activate the bonus fruit, based on arithmetic sequence of 9,19,29,39...
  -- formula : an = a1 + (n -1) * d, where a1 = 9, d = 10
  | score == 9 + (difficulty - 1) * 10 = state {bonusFruit = True}
  
  -- Increase the difficulty by 1 and score is added by 1 more every 10 Score
  -- So, every 10 score the score will be added by 2 in total. 
  -- It is necessary to avoid continuous addtion to the difficulty when the score amount is still a multiple of 10.
  | score == 0 = state -- To avoid score start at 1
  | score `mod` 10 == 0 = state { difficulty = difficulty + 1, 
                                  score = score + 1, 
                                  bonusFruit = False}
  | otherwise = state


randomVector :: Vector -> StdGen -> (Vector, StdGen)
randomVector (width, height) inputStdGen  = 
  (element, stdGen)
  where indexX = randomR (1, width) inputStdGen
        indexY = randomR (1, height) inputStdGen
        posX              = fst indexX
        posY              = fst indexY
        stdGen            = snd indexX
        element           = (posX, posY)