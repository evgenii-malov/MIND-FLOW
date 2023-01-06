{-
  stocks prediction: https://www.hackerrank.com/challenges/crosswords-101/
  Author: Evgeniy Malov <evgeniiml@gmail.com>
  Date: Jan 3, 2023
  join me : https://www.youtube.com/@EvgeniyMalov
            https://ru.linkedin.com/in/deepwalk
-}

import qualified Data.Vector.Unboxed as U
import qualified Data.Ord as O
import Debug.Trace
import Data.List
import Data.List.Split
import qualified Data.List as L
import Data.Maybe
import Control.Monad

type X = Int
type Y = Int
type GI = Int
type SLength = Int
-- type 
-- data Slot = Slot X Y Dir Length
data Slot = Slot GI Dir SLength deriving (Eq,Show)
data Dir = Vert | Horiz  deriving (Eq,Show)

slot_len (Slot _ _ l) = l

slotToGis :: Slot -> [GI]
slotToGis (Slot gi d l) | d == Vert = (\dy->gi+dy*10) <$> [0..l-1]
                        | d == Horiz = (\dx->gi+dx) <$> [0..l-1]


toXY :: GI -> (Y,X)
toXY gi = divMod gi 10

toGi :: (Y,X) -> Int
toGi (y,x) = (y)*10+(x)

-- <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
markSlot :: Slot -> U.Vector Bool -> U.Vector Bool
markSlot s iv = iv U.// ((\gi -> (gi,True)) <$> gis)
    where
        gis = slotToGis s

sEmpty :: [String] -> U.Vector Bool -> GI -> Bool
sEmpty s vlock gi = (((s !! y) !! x) == '-') && (vlock U.! gi == False)
    where
        (y,x) = toXY gi

fst3 (a,_,_) = a

--solve :: [String] -> [String]
slots :: [String] -> [Slot]
slots p = fst3 (foldl it ([],U.replicate 100 False,U.replicate 100 False) [0..99])
    where
        it :: ([Slot], U.Vector Bool, U.Vector Bool) -> Int -> ([Slot], U.Vector Bool, U.Vector Bool)
        it (slots,markedX,markedY) gi = (slots',markedX',markedY')
            where
                slots' = fsy++fsx++slots
                s_v = Slot gi Vert ly
                s_h = Slot gi Horiz lx
                fsy = [s_v | ly > 0]
                fsx = [s_h | lx > 0]
                markedX' = if lx>0 then markSlot s_h markedX else markedX
                markedY' = if ly>0 then markSlot s_v markedY else markedY
                ly = len_y gi
                lx = len_x gi
                len_y gi = go 1 gi
                           where
                            go c gi | not $ sEmpty p markedY (gi) = c - 1
                                    | gi > 89 = c
                                    | otherwise = go (c+1) (gi+10)
                len_x gi = go 1 gi
                           where
                            go c gi | not $ sEmpty p markedX (gi) = c - 1
                                    | gi `mod` 10 == 9 = c
                                    | otherwise = go (c+1) (gi+1)


solve :: [String] -> [String] -> [String]
solve area words = fromJust $ head $ dropWhile isNothing $ check <$> sequence combs
    where
        
        combs:: [[(Slot,String)]]
        combs = (\w -> [(s,w) | s <- (slots area), slot_len s == length w ] ) <$> swords
        swords = sortOn (O.Down . length) words        

        check :: [(Slot,String)] -> Maybe [String]
        check comb = foldM place area comb
        place ar ((Slot gi d l),word) 
                | d == Vert = go_v ar gi word
                | d == Horiz = go_h ar gi word
                where 
                    

                go_v = go (+10)
                go_h = go (+1)

                go step s gi [] = Just $ s
                go step s gi (c:word) 
                        | isJust $ tryp c = go step s' (step gi) word
                        | otherwise = Nothing
                        where
                            (Just s') = tryp c
                            tryp c 
                                | ch == '-' || ch == c = Just ns
                                | otherwise = Nothing
                                where
                                    ch = (s L.!! y) L.!! x
                                    ns = replace_ s y (replace_ (s L.!! y) x c)
                                    (y,x) = toXY gi 


replace_ :: [a] -> Int -> a -> [a]
replace_ ls p nc | 0<=p && p < length ls = (init $ take (p+1) ls) ++ [nc] ++ (drop (p+1) ls)
                | otherwise = error "index range error in replace"  



render :: Show a => [a] -> IO ()
render ls = sequence_ $ putStrLn.show <$> ls

rend :: [String] -> IO ()
rend ls = sequence_ $ putStrLn <$> ls


main = do  
         scr <- replicateM 10 getLine                     
         l' <- getLine
         let words = wordsBy (== ';') l'         
         rend $ solve (scr) (words)


sample =
        [
        "+-++++++++",
        "+-++++++++",
        "+-++++++++",
        "+-----++++",
        "+-+++-++++",
        "+-+++-++++",
        "+++++-++++",
        "++------++",
        "+++++-++++",
        "+++++-++++"
        ]

sample2 = [
    "+-++++++++",
    "+-++++++++",
    "+-------++",
    "+-++++++++",
    "+-++++++++",
    "+------+++",
    "+-+++-++++",
    "+++++-++++",
    "+++++-++++",
    "++++++++++"
    ]


words = ["LONDON","DELHI","ICELAND","ANKARA"]
words2 = ["AGRA","NORWAY","ENGLAND","GWALIOR"]

-- solve area words = sequence ws
--     where                
--         ss = slots area
--         ws :: [[(Slot,String)]]
--         ws = (\w -> [(s,w) | s <- ss, slot_len s == length w ] ) <$> swords
--         swords = sortOn (O.Down . length) Main.words
        


-- [[s7],[s6_1,s6_2],[s5]]
-- [[w7_1],[w6_1,w6_2],[w5_1]]

-- [[s7],  [s6_1,s6_2],[s6_1,s6_2],[s5]]
-- [[w7_1],[w6_1],     [w6_2],    [w5_1]]

-- [(s7,w7_1),(s6_1,w6_1),(s6_1,w6_2),(s5,w5_1)],
-- [(s7,w7_1),(s6_2,w6_1),(s6_1,w6_2),(s5,w5_1)],
-- [(s7,w7_1),(s6_1,w6_2),(s6_1,w6_2),(s5,w5_1)],
-- [(s7,w7_1),(s6_1,w6_2),(s6_2,w6_2),(s5,w5_1)],