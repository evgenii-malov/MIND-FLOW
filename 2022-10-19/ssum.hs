import Control.Monad
import Data.List
import Data.Array
import qualified Data.Vector.Primitive as V



countingSort :: (Ix n) => [n] -> n -> n -> [n]
countingSort l lo hi = concat [replicate times n | (n, times) <- counts]
  where counts = assocs (accumArray (+) 0 (lo, hi) [(i, 1) | i <- l])
  
f :: [Int] -> Integer -> Int
f es n = go es n 0
    where        
        go [] n c = if n > 0 then (-1) else c
        go (e:es) n c | n <= 0 = c
                      | otherwise = go es (n-(fromIntegral e)) (c+1)   


-- https://en.wikipedia.org/wiki/Binary_search_algorithm                      
-- right most
bsearch :: Int -> V.Vector Int -> Int
bsearch e v = go 0 ((V.length v)-1)
    where go f t | f == t = f
                 | (mid == e) = i_mid 
                 | e > mid = go (i_mid+1) t
                 | otherwise = go f i_mid
                 where
                    i_mid = f + ((t-f) `div` 2)
                    mid = v V.! i_mid    

f' :: V.Vector Int -> Int -> Int    
f' v s | (i == li) && ((v V.! li) >= s) = li + 1
       | (i == li) && ((v V.! li) < s) = -1
       | (i == 0) = 1
       | otherwise = i+1
        where
          i = bsearch s v
          li = (V.length v)-1

main = do
        getLine
        es' <- getLine
        let es'' = reverse $ sort $ (read <$> words es') :: [Int]
        let es = tail $ scanl (+) 0 es''
        let ves = V.fromList es
        
        tn <- readLn
        xs <- replicateM tn readLn
        forM_ xs (\s -> putStrLn $ show $ f' ves s) -- f es'' s
                          
