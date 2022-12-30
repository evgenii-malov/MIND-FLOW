import qualified Data.Vector.Unboxed as U
import qualified Data.Ord as O
import Data.Bool (Bool)
import Debug.Trace
import Data.List.Extra
import SizedSeq (ssElts)

type X = Int
type Y = Int
type GI = Int
type SLength = Int
-- type 
-- data Slot = Slot X Y Dir Length
data Slot = Slot GI Dir SLength deriving (Eq,Show)
data Dir = Vert | Horiz  deriving (Eq,Show)

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
                            go c gi | not $ sEmpty p markedY (traceShowId gi) = c - 1
                                    | gi > 89 = c
                                    | otherwise = go (c+1) (gi+10)
                len_x gi = go 1 gi
                           where
                            go c gi | not $ sEmpty p markedX (traceShowId gi) = c - 1
                                    | gi `mod` 10 == 9 = c
                                    | otherwise = go (c+1) (gi+1)

--  [s | s@(Slot _ _ l) <- slots sample , l>1]

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


words = ["LONDON","DELHI","ICELAND","ANKARA"]

--solve :: [String] -> [String] -> [String]
solve area words = head $ dropWhile isNothing $ check <$> sequence space
    where
    	check :: [(Slot,String)] -> Maybe [String]
	check comb = foldM place area comb
	place ar ((Slot gi d l),word) 
		    | d == Vert = go_v ar gi word
		    | d == Horiz = go_h ar gi word
		    where 
	            -- go :: [String] -> GI -> Maybe [String]		      
		    gi_v s gi [] = Just $ s
		    go_v s gi (c:word) | isJust $ tryp c = go_v s' (gi+10) word
				       | otherwise = Nothing
				       where
				       	(Just s') = tryp c
					tryp c | ch == '-' || ch == c = Just ns
					       | otherwise = Nothing
					  where
					    ch = (s ! y) ! x
					    ns = replace s y (replace (s ! y) x c)
					    (y,x) = toXY gi 




        space = zipWith z gs gw
        z ss ws = do s<-ss
                     w<-ws
                     return (s,w)   

        gs = groupOn (\(Slot _ _ l)->l) (sortBy (\(Slot _ _ l1) (Slot _ _ l2) -> compare (O.Down l1) (O.Down l2)) fss)
        gw = groupOn length (sortOn (O.Down . length) Main.words)
        ss = slots area
        fss = [s |s@(Slot _ _ l) <- ss, l `elem` (length <$> words)]




