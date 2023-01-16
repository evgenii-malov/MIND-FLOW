{-
  matrix rotation: https://www.hackerrank.com/challenges/matrix-rotation/problem
  Author: Evgeniy Malov <evgeniiml@gmail.com>
  Date: Jan 15, 2023
  join me : https://www.youtube.com/@EvgeniyMalov
            https://ru.linkedin.com/in/deepwalk
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where

import qualified Data.Matrix as M
import Control.Monad
import Data.Matrix (toLists)
import qualified Data.Map as Mp

type X = Int -- [0..]
type Y = Int -- [0..]
type Height = Int -- max coord + 1
type Width = Int

type InitHeight = Int
type InitWidth = Int

data Side = Left | Right | Top | Bottom deriving (Eq,Show)



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
    | x == lx && y<ry = Main.Left
    | x<rx && y==ry = Main.Bottom
    | x == rx && y>ly = Main.Right
    | lx<x && y == ly = Main.Top
    where
        (ly,lx,ry,rx) = subrect_c y x ih iw

newpos :: Y -> X -> InitHeight -> InitWidth -> Int -> (Y,X)
newpos y x ih iw r = go y x active_l
    where
        go y x 0 = (y,x)
        go y x l | s y x == Main.Left = go (y+1) x (l-1)
                 | s y x == Main.Bottom = go y (x+1) (l-1)
                 | s y x == Main.Right = go (y-1) x (l-1)
                 | s y x == Main.Top = go y (x-1) (l-1)
            where
              s y x = side y x ih iw
        active_l = r `mod` perimiter
        perimiter = r_w*2+r_h*2-4
        (r_h,r_w) = subrect y x ih iw


cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- solve :: M.Matrix Int -> Int -> M.Matrix Int
-- solve m r = foldl f zm (cartProd [1..h] [1..w])
--     where
--         f mx (y,x) = M.setElem orig (np_y+1,np_x+1) mx
--             where
--                 (np_y,np_x) = newpos (y-1) (x-1) h w r
--                 orig = m M.! (y,x)
--         zm = M.zero h w
--         h = M.nrows m
--         w = M.ncols m


solve :: M.Matrix Int -> Int -> M.Matrix Int
solve m r = M.matrix h w g 
    where 
        g (y,x) = m M.! (y',x')
            where 
                (y',x') = d Mp.! (y,x)
        
        d = Mp.fromList $ [(n (y-1) (x-1) , (y,x) ) | (y,x) <- (cartProd [1..h] [1..w])]
            where   
                n y x = (ny+1,nx+1)
                    where
                        (ny,nx) = newpos y x h w r

        h = M.nrows m
        w = M.ncols m


m = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
mM = M.fromLists m


main :: IO ()
main = do
        l <- getLine
        let [h, w, r] = (read :: String -> Int) <$> words l
        ls' <- replicateM h getLine
        let ls = (\l -> (read :: String -> Int) <$> words l) <$> ls'
        let sm = solve (M.fromLists ls) r
        forM_ (toLists sm) (\l -> putStrLn $ unwords $ show <$> l)

