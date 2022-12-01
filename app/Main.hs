module Main where

import Sokoban.Data ( 
    getCharLook
  , printStage
  , readStage
  , Stage )
import Sokoban.Logic ( 
    finishStage
  , move )

import Control.Monad (when)


game stage = game' stage []
    where
        game' stage [] = game' stage [stage]
        game' stage prev@(l:ls) = do
            printStage stage
            if finishStage stage then do
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

gameStages :: [Stage] -> IO ()
gameStages [] = putStrLn "You win!"
gameStages (stage:stages) = do
    clear <- game stage
    when clear $ putStrLn "Next Stage" >> gameStages stages

main :: IO ()
main = do
    stages <- mapM (readStage . ("stage" ++) . show) [1..10]
    gameStages stages

