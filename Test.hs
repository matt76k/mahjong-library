import Game.Mahjong.Data
import Game.Mahjong.Convert
import Game.Mahjong.Tools
import Data.List as L

main = do
        walls <- makeWalls
        let hand = L.sort $ take 13 walls
        print $ convertFrom $ concatMap show hand
        print hand
        print $ map isTANYAO hand
