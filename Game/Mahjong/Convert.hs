module Game.Mahjong.Convert (
  convertFrom
) where

import Game.Mahjong.Data

convertFrom (h:[]) = [convert h False]
convertFrom (h:t) = if head t == '赤' then convert h True : convertFrom (tail t) else convert h False : convertFrom t

convert c b = case c of
                '一' -> WAN 1 b
                '二' -> WAN 2 b
                '三' -> WAN 3 b
                '四' -> WAN 4 b
                '五' -> WAN 5 b
                '六' -> WAN 6 b
                '七' -> WAN 7 b
                '八' -> WAN 8 b
                '九' -> WAN 9 b
                '①' -> PIN 1 b
                '②' -> PIN 2 b
                '③' -> PIN 3 b
                '④' -> PIN 4 b
                '⑤' -> PIN 5 b
                '⑥' -> PIN 6 b
                '⑦' -> PIN 7 b
                '⑧' -> PIN 8 b
                '⑨' -> PIN 9 b
                '１' -> SOU 1 b
                '２' -> SOU 2 b
                '３' -> SOU 3 b
                '４' -> SOU 4 b
                '５' -> SOU 5 b
                '６' -> SOU 6 b
                '７' -> SOU 7 b
                '８' -> SOU 8 b
                '９' -> SOU 9 b
                '東' -> TON
                '南' -> NAN
                '西' -> SHA
                '北' -> PEI
                '白' -> HAKU
                '發' -> HATSU
                '中' -> CHUN
