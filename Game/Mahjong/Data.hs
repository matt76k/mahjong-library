module Game.Mahjong.Data
where

import Data.List as L
import Data.Random hiding (shuffle)
import Data.Random.Source.Std
import Data.Random.Extras

data WinType = TSUMO | RON

data KIND = MANZ
          | PINZ
          | SOUZ
          | WIND
          | SANG deriving (Show, Ord, Eq, Enum)

data Tile = WAN Int Bool | PIN Int Bool | SOU Int Bool 
          | TON | NAN | SHA | PEI
          | HAKU | HATSU | CHUN deriving Ord

instance Eq Tile where
    (WAN n1 _) == (WAN n2 _) = n1 == n2
    (PIN n1 _) == (PIN n2 _) = n1 == n2
    (SOU n1 _) == (SOU n2 _) = n1 == n2
    TON == TON = True
    NAN == NAN = True
    SHA == SHA = True
    PEI == PEI = True
    HAKU == HAKU = True
    HATSU == HATSU = True
    CHUN == CHUN = True
    _ == _ = False

instance Show Tile where
    show (WAN n True) = "五赤"
    show (PIN n True) = "⑤赤"
    show (SOU n True) = "５赤"
    show (WAN n d) = ["一", "二", "三", "四", "五", "六", "七", "八", "九"] !! (n - 1)
    show (PIN n d) = ["①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨"] !! (n - 1)
    show (SOU n d) = ["１", "２", "３", "４", "５", "６", "７", "８", "９"] !! (n - 1)
    show TON = "東"
    show NAN = "南"
    show SHA = "西"
    show PEI = "北"
    show HAKU = "白"
    show HATSU = "發"
    show CHUN = "中"

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