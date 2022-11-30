module Main where

import Sokoban.Data
import Sokoban.Logic


game stage = game' stage []
    where
        game' stage [] = game' stage [stage]
        game' stage prev@(l:ls) = do
            printStage stage
            if finishStage stage then do
                putStrLn "You win!"
                return True
            else do
                dir <- getChar
                putStr "\n"
                case dir of
                    '\n' -> game' stage prev
                    'u' -> game' l ls
                    'q' -> return False
                    _   -> do
                        case getCharLook dir of
                            Just look -> game' (move look stage) (stage:prev)
                            Nothing -> game' stage prev

main :: IO ()
main = do
    stage2 <- readStage "stage2"
    game stage2
    return ()

