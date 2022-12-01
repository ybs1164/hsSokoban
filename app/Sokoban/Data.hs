module Sokoban.Data (
    Object (..)
  , Look (..)
  , Pos
  , Tile
  , Stage
  , getCharTile
  , getCharLook
  , getLookVector
  , readStage
  , printStage
) where

import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as Map


data Object = Empty | Wall | Rock | Goal | Player
    deriving Eq

data Look = W | A | S | D

type Pos = (Int, Int)

type Tile = [Object]

type Stage = Map Pos Tile


getTileEmoji :: Tile -> String
getTileEmoji [] = "⬜"
getTileEmoji (tile:tiles) = case tile of
    Empty  -> "  "
    Wall   -> "⬛"
    Goal   -> "💚"
    Player -> "️😎"
    Rock   -> "🔷"

getCharTile :: Char -> Tile
getCharTile c = case c of
    'w' -> [Wall]
    'r' -> [Rock]
    'g' -> [Goal]
    'p' -> [Player]
    'R' -> [Rock, Goal]
    'P' -> [Player, Goal]
    ' ' -> [Empty]
    _   -> []

getCharLook :: Char -> Maybe Look
getCharLook c = case c of
    'w' -> Just W
    'k' -> Just W

    'a' -> Just A
    'h' -> Just A

    's' -> Just S
    'j' -> Just S
    
    'd' -> Just D
    'l' -> Just D

    _   -> Nothing

getLookVector :: Look -> (Int, Int)
getLookVector look = case look of
    W -> (-1, 0)
    A -> (0, -1)
    S -> (1, 0)
    D -> (0, 1)

getStageString :: Stage -> String
getStageString stage =
    concatMap ((++"\n") . concatMap (getTileEmoji . snd)) 
    . groupBy (\x y -> (fst . fst) x == (fst . fst) y) 
    $ Map.toList stage


readStage :: FilePath -> IO Stage
readStage name = do
    handle <- openFile ("stages/" ++ name ++ ".txt") ReadMode
    contents <- hGetContents handle
    return . Map.fromList 
      . concat . indexing 
      . (map . map) getCharTile
      . lines $ filter (/='\r') contents
    where
    indexing =
        zipWith (\row xs -> zipWith (\col x -> ((row, col), x)) [0..] xs) [0..]

printStage :: Stage -> IO ()
printStage = putStr . getStageString
