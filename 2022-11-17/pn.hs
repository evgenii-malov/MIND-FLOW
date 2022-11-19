import Control.Monad
import Data.IntMap

pen :: Int -> Int
pen 1 = 1
pen n = (3*n-2) + (pen $ n - 1)

cached_pen :: IntMap Int -> Int -> (Int,IntMap Int)
cached_pen m 1 = (1,insert 1 1 m)
cached_pen m n | member n m = ((m ! n),m)
               | otherwise = (v,m')
               where
                   v = (3*n-2) + (fst $ cached_pen m (n-1))
                   m' = insert n v (snd $ cached_pen m (n-1)) 

pens :: Int -> [Int]
pens ln = go 1 []
            where
                go 1 _ = go 2 [1,0]   
                go n (p:r) | n <= ln = go (n+1) (((3*n-2)+p):p:r)
                           | otherwise = reverse $ p:r



fib n = take n fiblist
  where fiblist = 0:1:(zipWith (+) fiblist (tail fiblist))
  
pens' :: Int -> [(Int,Int)]
pens' ln = take ln plist 
                where 
                    plist = (1,1):(2,5):(zipWith g plist $ tail plist)           
                    g _ (pi,pv) = (pi+1,(3*(pi+1)-2)+pv)

main = do
    n <- readLn
    ts <- (replicateM n readLn) :: IO [Int]
    --let mp = fromList $ zip [0..] $ pens $ maximum ts    
    let mp = fromList $ pens' $ maximum ts    
    forM_ ts (\t -> putStrLn.show $ mp ! t)
    
    --foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
    --let sc = snd $ cached_pen empty $ maximum ts
    --foldM_ (\c t -> do 
    --                    let (v,c') = cached_pen c t
    --                    putStrLn $ show v
    --                    return c' ) empty ts
