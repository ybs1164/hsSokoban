module Main where

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

getStageString :: Stage -> String
getStageString stage =
    concatMap
        (\column -> concatMap (getTileEmoji . snd) column ++ "\n") $
        groupBy (\x y -> (fst . fst) x == (fst . fst) y) $
        Map.toList stage

getLookingTiles :: Pos -> Look -> Stage -> [Tile]
getLookingTiles pos look stage =
    map (\(Just a) -> a) $
        takeWhile (not . isWall) $
        map (`Map.lookup` stage) $
        getLooking pos look
    where
        isWall (Just objects) = Wall `elem` objects
        isWall Nothing  = True

getLooking :: Pos -> Look -> [Pos]
getLooking (x, y) look = let (dx, dy) = getLookVector look in
    zip [x, x+dx..] [y, y+dy..]

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
        zip [(x, y-1) | x <- [0..], y <- [1..length $ head xss]] (concat xss)

printStage :: Stage -> IO ()
printStage = putStr . getStageString

finishStage :: Stage -> Bool
finishStage =
    (== Map.empty) . Map.filterWithKey checkAloneRock
    where
        checkAloneRock _ v = Rock `elem` v && Goal `notElem` v


findPlayer :: Stage -> Pos
findPlayer stage =
    (fst . head) (filter (\(_, objects) -> Player `elem` objects) (Map.toList stage))

moveTiles :: [Tile] -> [Tile]
moveTiles [] = []
moveTiles [x] = [x]
moveTiles (x:y:zs) =
    if not (any isPush y) then
        filter (not . isPush) x : (filter isPush x ++ y) : zs
        else let (tile:tiles) = moveTiles (y:zs) in
            filter (not . isPush) x : (filter isPush x ++ tile) : tiles
    where
        isPush Rock = True
        isPush _    = False

canMove :: [Tile] -> Bool
canMove = not . all (any isPush)
    where
        isPush Rock = True
        isPush _    = False

movePlayer :: Tile -> [Tile] -> [Tile]
movePlayer p []    = [p]
movePlayer p tiles =
    if (not . canMove) tiles then
        p:tiles
        else let (t:ts) = moveTiles tiles in
        filter (not . isPlayer) p : (filter isPlayer p ++ t) : ts
    where
        isPlayer Player = True
        isPlayer _      = False

move :: Look -> Stage -> Stage
move look stage = let pos = findPlayer stage;
                      (player: tiles) = getLookingTiles pos look stage in
    foldr (uncurry Map.insert) stage (zip (getLooking pos look) $ movePlayer player tiles)


game :: Stage -> IO ()
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

