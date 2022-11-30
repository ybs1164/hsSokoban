module Main where

import Sokoban.Data
import Sokoban.Logic

game stage = do
    printStage stage
    if finishStage stage then
        putStrLn "You win!"
    else do
        (dir:_) <- getLine
        if dir == 'q' then
            return ()
        else do
            let look = getCharLook dir
            game $ move look stage


main :: IO ()
main = do
    stage1 <- readStage "stage1"
    game stage1

