import Game.Mahjong.Data
import Game.Mahjong.Convert
import Game.Mahjong.Tools
import Data.List as L

main = do
        walls <- makeWalls
        print $ convertFrom $ concatMap show $ L.sort $ take 13 walls
        print $ L.sort $ take 13 walls
