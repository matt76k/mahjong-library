module Game.Mahjong.Tools (
  makeWalls
) where

import Game.Mahjong.Data
import Data.List as L
import Data.Random hiding (shuffle)
import Data.Random.Source.Std
import Data.Random.Extras

makeWalls :: (MonadRandom m) => m [Tile]
makeWalls = runRVar (shuffle $ five ++ noFive) StdRandom
  where
    makeTiles kind = map (`kind` False)
    makeRedTiles kind = map (`kind` True)
    withoutFive = concatMap (`makeTiles` L.delete 5 [1..9]) [WAN, PIN, SOU]
    withoutRedFive = concatMap (`makeTiles` [5, 5, 5]) [WAN, PIN, SOU]
    redFive = concatMap (`makeRedTiles` [5]) [WAN, PIN, SOU]
    windAndSang = [TON, NAN, SHA, PEI, HAKU, HATSU, CHUN]
    five = withoutRedFive ++ redFive
    noFive = concat $ replicate 4 $ withoutFive ++ windAndSang
