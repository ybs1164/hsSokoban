{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Main where

data Tile = Empty | Wall | Goal | Player | Rock

type Map = [[Tile]]

testMap = [
    [Empty, Wall, Wall, Empty],
    [Wall, Empty, Goal, Wall],
    [Wall, Goal, Rock, Wall],
    [Wall, Player, Empty, Wall],
    [Empty, Wall, Wall, Empty]]

getTileString :: Tile -> String
getTileString t = case t of
    Empty  -> "⬜"
    Wall   -> "⬛"
    Goal   -> "💚"
    Player -> "️😎"
    Rock   -> "🔷"

printMap :: Map -> IO ()
printMap [] = return ()
printMap (ts:tss) = putStrLn (concatMap getTileString ts) >> printMap tss



main :: IO ()
main = putStrLn "Hello, Haskell!"

