module Main where

import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as Map

data Object = Empty | Wall | Rock | Goal | Player
    deriving (Eq, Show)

data Look = W | A | S | D

type Pos = (Int, Int)

type Stage = Map Pos [Object]


getObjectEmoji :: [Object] -> String
getObjectEmoji [] = "⬜"
getObjectEmoji (object:objects) = case object of
    Empty  -> "⬜"
    Wall   -> "⬛"
    Goal   -> "💚"
    Player -> "️😎"
    Rock   -> "🔷"

getStringObject :: Char -> [Object]
getStringObject s = case s of
    'w' -> [Wall]
    'r' -> [Rock]
    'g' -> [Goal]
    'p' -> [Player]
    _   -> []


readStage :: FilePath -> IO Stage
readStage name = do
    handle <- openFile ("stages/" ++ name ++ ".txt") ReadMode
    contents <- hGetContents handle
    return $ Map.fromList $ indexing $ map (map getStringObject) $ words contents
    where
    indexing xss = 
        zip [(x, y-1) | x <- [0..], y <- [1..length $ head xss]] (concat xss)

getStageString :: Stage -> String
getStageString stage =
    concatMap
        (\column -> concatMap (getObjectEmoji . snd) column ++ "\n") $
        groupBy (\x y -> (fst . fst) x == (fst . fst) y) $
        Map.toList stage

printStage :: Stage -> IO ()
printStage = putStr . getStageString

getLookVector :: Look -> (Int, Int)
getLookVector look = case look of
    W    -> (-1, 0)
    A  -> (0, -1)
    S  -> (1, 0)
    D -> (0, 1)

findPlayer :: Stage -> Pos
findPlayer stage = let list_stage = Map.toList stage in
    (fst . head) (filter (\((x, y), obj:_) -> obj == Player) list_stage)


getObjectsLooking :: Pos -> Look -> Stage -> [(Pos, [Object])]
getObjectsLooking (x, y) look stage = let (dx, dy) = getLookVector look in
    zip (zip [x, x+dx..] [y, y+dy..]) $
        map (\(Just a) -> a) $
        takeWhile (not . isWall) $ 
        zipWith (curry (`Map.lookup` stage)) [x, x+dx..] [y, y+dy..]
    where
        isWall (Just objects) = Wall `elem` objects
        isWall Nothing  = True


canMove :: Pos -> Look -> Stage -> Bool
canMove pos look =
    elem [] . map snd . getObjectsLooking pos look


-- todo : finish this function
pushObjects :: [(Pos, [Object])] -> [(Pos, [Object])]
pushObjects [] = []
pushObjects [x] = [x]
pushObjects (x:y:zs) =
    if isAllStand (snd y) then
        (fst y, snd x ++ snd y):zs
        else let tiles = pushObjects (y:zs) in
            if null tiles then
                []
                else (fst y, snd (head tiles) ++ snd y) : tail tiles
    -- foldr (\((_, objects), (pos, prev)) ls -> (pos, objects ++ prev): ls) [head lives]
    where
        isAllStand [] = True
        isAllStand xs = (not . any isPush) xs
        isPush Rock = True
        isPush _ = False


move :: Look -> Stage -> Stage
move look stage = let pos = findPlayer stage in
    if canMove pos look stage then
        let lookingObjects = getObjectsLooking pos look stage;
            positionObjects = ((0, 0), []): lookingObjects;
            movementList = reverse (zip positionObjects (tail positionObjects)) in
            foldr (\((_, objects), (pos, _)) s -> Map.insert pos objects s) stage movementList
        else stage


main :: IO ()
main = putStrLn "Hello, Haskell!"

