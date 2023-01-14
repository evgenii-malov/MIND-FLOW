import Control.Monad

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Control.Monad
import Debug.Trace

-- preprocessing O(n*log n) | sparse table | ranges size power of 2
-- query O(1)

-- a is init array
-- f [i,k] = min ( a[i,i+2^k] )
-- f [i,k] = min (f [i,k-1], f [i+2^(k-1),k-1])
-- m (l,r) = min (f (l,k'), f (r-2^k'+1,k')) where k' = log (r-l)

--prep :: [Int] -> IO (MV.IOVector Int)
indx l i k = k*l+i

prep l = do 
          v <- MV.new $ (length l)*(mk+1)          
          forM ps (act v)
          return v
          --print mvec
          v1 <- V.freeze v
          --print v1
          return v1
         where 
            act v (k,i) | k == 0 = do MV.write v (ind i k) (iv V.! (ind i 0))
                        | otherwise = do
                                        p1 <- MV.read v (ind i (k-1)) 
                                        p2 <- MV.read v (ind i' (k-1))
                                        MV.write v (ind i k) (min p1 p2)
                                        where
                                          i' = i + 2^(k-1)

            mk = log2 (length l)
            ps = do {k <- [0..mk];i <- [0..(length l)-1];return (k,i)}
            iv = V.fromList l
            ind = indx (length l)
            


log2 :: Int -> Int
log2 n = floor ( logBase 2 (fromIntegral n))

rmq :: (V.Vector Int) -> Int -> Int -> Int -> IO Int
rmq v arr_l l r = do
                --print "start ex wpv"
                --v <- wpv
                --print "done"
                --print "start log calc"
                let k' = log2 (r-l+1)
                let i' = (r-2^k'+1)
                --print "done"
                let mp1 = v V.! (indx arr_l l k')
                let mp2 = v V.! (indx arr_l i' k')
                return $ min mp1 mp2
               --where 
                  --i' = (r-2^k'+1)
                  --k' = log2 (r-l+1)


main = do
        fl <- getLine
        let (la:t_cnt:[]) = read <$> (words fl)
        --la <- readLn
        --t_cnt <- readLn
        l <- getLine
        let arr = (read <$> words l) :: [Int]
        pr <- prep arr
        ts' <- (replicateM t_cnt getLine)
        let ts = ((\l -> (read <$> words l) :: [Int] ) <$> ts') :: [[Int]]
        
        forM_ ts (\t -> (do                            
                             let (l:r:[]) = t
                             mn <- rmq pr la l r
                             putStrLn $ show mn
                           ))
        

        

-- main_ = do
--         la <- readLn
--         t_cnt <- readLn
--         l <- getLine
--         let arr = (read <$> words l) :: [Int]
--         let pr = prep arr
--         replicateM_ t_cnt (do
--                              t' <- getLine
--                              let t = (read <$> words t') :: [Int]
--                              let (l:r:[]) = t
--                              mn <- rmq pr la l r
--                              putStrLn $ show mn
--                            )