import qualified Data.Matrix as M

type X = Int -- [0..]
type Y = Int -- [0..]
type Height = Int -- max coord + 1
type Width = Int 

type InitHeight = Int
type InitWidth = Int 

data Side = Left | Right | Top | Bottom deriving (Eq,Show)


subr :: X -> Y -> InitHeight -> InitWidth -> (Height,Width)
subr x y ih iw = (sh,sw)
    where
        sw | mn<=xmid = iw - 2*mn
           | otherwise = iw - 2*(iw - 1 - mx)
        sh | mn<=ymid = ih - 2*mn
           | otherwise = ih - 2*(ih - 1 - mx)  
        mn = min x y    
        mx = max x y    
        xmid = iw `div` 2
        ymid = ih `div` 2


subrect :: Y -> X -> InitHeight -> InitWidth -> (Height,Width)        
subrect y x ih iw = go 0 0 (ih-1) (iw-1) ih iw 
    where
       go ly lx ry rx h w | x == lx || x == rx || y == ly || y == ry = (h,w)
                          | otherwise = go (ly+1) (lx+1) (ry-1) (rx-1) (h-2) (w-2)

subrect_c :: Y -> X -> InitHeight -> InitWidth -> (Y,X,Y,X)        
subrect_c y x ih iw = go 0 0 (ih-1) (iw-1) ih iw 
    where
       go ly lx ry rx h w | x == lx || x == rx || y == ly || y == ry = (ly,lx,ry,rx)
                          | otherwise = go (ly+1) (lx+1) (ry-1) (rx-1) (h-2) (w-2)


-- 1 1 1 1  0          
-- 1 1 1 1  1          
-- 1 1 1 1  2          
-- 1 1 1 1  3          
-- 1 1 1 1  4          
-- 1 1 1 1  5  

-- 0 1 2 3 

side :: Y -> X -> InitHeight -> InitWidth -> Side
side y x ih iw 
    | x == lx && y<ry = Left
    | x<rx && y==ry = Bottom
    | x == rx && y>ly = Right
    | lx<x && y == ly = Top
    where
        (ly,lx,ry,rx) = subrect_c y x ih iw

newpos :: Y -> X -> InitHeight -> InitWidth -> Int -> (Y,X)
newpos y x ih iw r = go y x active_l
    where
        go y x 0 = (y,x)
        go y x l | s y x == Left = go (y+1) x (l-1)
                 | s y x == Bottom = go y (x+1) (l-1)
                 | s y x == Right = go (y-1) x (l-1)
                 | s y x == Top = go y (x-1) (l-1)
            where 
              s y x = side y x ih iw
        active_l = r `mod` perimiter
        perimiter = r_w*2+r_h*2
        (r_h,r_w) = subrect y x ih iw
        

cartProd xs ys = [(x,y) | x <- xs, y <- ys]

solve :: M.Matrix Int -> Int -> M.Matrix Int
solve m r = foldl f zm (cartProd [1..h] [1..w])
    where 
        f mx (y,x) = M.setElem orig (np_y+1,np_x+1) mx
            where
                (np_y,np_x) = newpos (y-1) (x-1) h w r
                orig = m M.! (y,x)                        
        zm = M.zero h w
        h = M.nrows m
        w = M.ncols m
    
        --r | x + active_path <= r_w = (x + active_path,y)
        --  | x + active_path <= (r_w+r_h) = (r_w,(x + active_path)-r_w)
        --  | x + active_path <= (2*r_w+r_h) = ((2*r_w+h) - (x + active_path),1)
        --  | otherwise = (1, (x + active_path) - (2*r_w+h) )  
        
        

m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
mM = M.fromLists m