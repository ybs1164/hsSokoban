module Main where

import Sokoban.Data
import Sokoban.Logic


game stage = do
    printStage stage
    if finishStage stage then do
        putStrLn "You win!"
        return True
    else do
        dir <- getChar
        putStr "\n"
        case dir of
            '\n' -> game stage
            'q' -> return False
            _   -> do
                case getCharLook dir of
                    Just look -> game $ move look stage
                    Nothing -> game stage

main :: IO ()
main = do
    stage2 <- readStage "stage2"
    game stage2
    return ()

