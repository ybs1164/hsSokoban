module Sokoban.Logic where

import Sokoban.Data (
    getLookVector
  , Look
  , Object(Player, Wall, Goal, Rock)
  , Pos
  , Stage
  , Tile )

import Data.Maybe ( catMaybes, isJust )
import Data.Map (Map)
import qualified Data.Map as Map


getLookingTiles :: Pos -> Look -> Stage -> [Tile]
getLookingTiles pos look stage =
    takeWhile (not . any isWall)
    . catMaybes
    . takeWhile isJust
    . map (`Map.lookup` stage)
    $ getLooking pos look
    where
        isWall Wall = True
        isWall _    = False

getLooking :: Pos -> Look -> [Pos]
getLooking (x, y) look = let (dx, dy) = getLookVector look in
    zip [x, x+dx..] [y, y+dy..]

finishStage :: Stage -> Bool
finishStage =
    (== Map.empty) . Map.filterWithKey checkAloneRock
    where
        checkAloneRock _ v = Rock `elem` v && Goal `notElem` v


findPlayer :: Stage -> Pos
findPlayer stage =
    fst . head . filter ((Player `elem`) . snd)
    $ Map.toList stage

moveTiles :: [Tile] -> [Tile]
moveTiles [] = []
moveTiles [x] = [x]
moveTiles xs@(x:y:zs)
  | not (any isPush x) = xs
  | not (any isPush y) =
        filter (not . isPush) x : (filter isPush x ++ y) : zs
  | otherwise = let (tile:tiles) = moveTiles (y:zs) in
        filter (not . isPush) x : (filter isPush x ++ tile) : tiles
    where
        isPush Rock = True
        isPush _ = False

canMove :: [Tile] -> Bool
canMove = not . all (any isPush)
    where
        isPush Rock = True
        isPush _    = False

movePlayer :: Tile -> [Tile] -> [Tile]
movePlayer p []    = [p]
movePlayer p tiles =
    if (not . canMove) tiles then
        p : tiles
    else let (t:ts) = moveTiles tiles in
        filter (not . isPlayer) p : (filter isPlayer p ++ t) : ts
    where
        isPlayer Player = True
        isPlayer _      = False

move :: Look -> Stage -> Stage
move look stage = let pos = findPlayer stage
                      (player:tiles) = getLookingTiles pos look stage in
    foldr (uncurry Map.insert) stage
    . zip (getLooking pos look)
    $ movePlayer player tiles

