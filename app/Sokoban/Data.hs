module Sokoban.Data (
    Object (..)
  , Look (..)
  , Pos
  , Tile
  , Stage
  , getTileEmoji
  , getCharTile
  , getCharLook
  , getLookVector
  , getStageString
  , readStage
  , printStage
) where

import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as Map


data Object = Empty | Wall | Rock | Goal | Player
    deriving (Eq, Show)

data Look = W | A | S | D

type Pos = (Int, Int)

type Tile = [Object]

type Stage = Map Pos Tile


getTileEmoji :: Tile -> String
getTileEmoji [] = "⬜"
getTileEmoji (tile:tiles) = case tile of
    Empty  -> "⬜"
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
    _   -> []

getCharLook :: Char -> Look
getCharLook c = case c of
    'w' -> W
    'a' -> A
    's' -> S
    'd' -> D
    _   -> D

getLookVector :: Look -> (Int, Int)
getLookVector look = case look of
    W -> (-1, 0)
    A -> (0, -1)
    S -> (1, 0)
    D -> (0, 1)


readStage :: FilePath -> IO Stage
readStage name = do
    handle <- openFile ("stages/" ++ name ++ ".txt") ReadMode
    contents <- hGetContents handle
    return $ Map.fromList $ indexing $ map (map getCharTile) $ words contents
    where
    indexing xss =
        zip [(x, y-2) | x <- [0..], y <- [1..length $ head xss]] (concat xss)


getStageString :: Stage -> String
getStageString stage =
    concatMap
        (\column -> concatMap (getTileEmoji . snd) column ++ "\n") $
        groupBy (\x y -> (fst . fst) x == (fst . fst) y) $
        Map.toList stage


printStage :: Stage -> IO ()
printStage = putStr . getStageString
