import Data.List
import qualified Data.Tree as T


data Tree a = Node a [Tree a] deriving (Show)

type FocusTree a = (Tree a, [Int]) 

tr :: Tree Integer -> T.Tree Integer
tr (Node a childs) = T.Node a $ tr <$> childs

visitLeft :: FocusTree a -> FocusTree a
visitLeft (t,d) = (t,(init d)++[(last d)-1])

visitRight :: FocusTree a -> FocusTree a
visitRight (t,d) = (t,(init d)++[(last d)+1])

visitParent :: FocusTree a -> FocusTree a
visitParent (t,d) = (t,init d)

visitChild :: FocusTree a -> Int -> FocusTree a
visitChild (t,d) n = (t,d++[n])

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

insertLeft :: FocusTree Int -> Int -> FocusTree Int
insertLeft (t,d) n = (il t d,(init d)++[(last d)+1]) 
    where 
        il (Node a childs) [ci] = Node a $ insertAt (Node n []) (ci-1) childs 
        il (Node a childs) (ci:ls) = Node a $ insertAt (il t ls) (ci-1) childs  
            where t = childs !! (ci-1)

--t = (Node 0 [Node 1 []],[1])
--insertLeft ft 2

insertRight :: FocusTree Int -> Int -> FocusTree Int
insertRight (t,d) n = (il t d, d) 
    where 
        il (Node a childs) [ci] = Node a $ insertAt (Node n []) (ci) childs 
        il (Node a childs) (ci:ls) = Node a $ insertAt (il t ls) (ci-1) childs  
            where t = childs !! (ci-1)
            
insertChild :: FocusTree Int -> Int -> FocusTree Int
insertChild (t,d) n = (il t d, d) 
    where 
        il (Node a childs) [] = Node a $ ((Node n []):childs)
        il (Node a childs) (ci:ls) = Node a $ replaceNth (ci-1) (il t ls) childs  
            where t = childs !! (ci-1)            

--deleteAt idx xs = lft ++ rgt
--  where (lft, (_:rgt)) = splitAt idx xs
deleteAt i items = take i items ++ drop (1 + i) items

del :: FocusTree Int -> FocusTree Int
del (t,d) = (del' t d, init d) 
    where 
        del' (Node a childs) [ci] = Node a $ deleteAt (ci-1) childs 
        del' (Node a childs) (ci:ls) = Node a $ replaceNth (ci-1) (del' t ls) childs  
            where t = childs !! (ci-1)            


changeV :: FocusTree Int -> Int -> FocusTree Int
changeV (t,d) v = (ch t d, d) 
    where         
        ch (Node a childs) [] = Node v childs
        ch (Node a childs) (ci:ls) = Node a $ replaceNth (ci-1) (ch t ls) childs  
            where t = childs !! (ci-1)  
            
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs


printT :: FocusTree Int -> IO ()
printT (t,d) = go t d
    where         
        go (Node a _) [] = putStrLn $ show a
        go (Node a childs) (ci:ls) = go t ls
            where t = childs !! (ci-1)  
            

-- | Analogue of @('Prelude.until')@
-- Yields the result of applying f until p holds.
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v 
    | p v       = return v
    | otherwise = f v >>= iterateUntilM p f
    


handle :: FocusTree Int -> String -> IO (FocusTree Int)
handle ft s | isPrefixOf "prt" s = do {print ft; return ft}
            | isPrefixOf "print" s = do {printT ft;return ft}
            | isPrefixOf "change" s = do {return $ changeV ft arg1}
            | isPrefixOf "visit left" s = do {return $ visitLeft ft}
            | isPrefixOf "visit right" s = do {return $ visitRight ft}
            | isPrefixOf "visit child" s = do {return $ visitChild ft arg2}
            | isPrefixOf "visit parent" s = do {return $ visitParent ft}
            | isPrefixOf "insert child" s = do {return $ insertChild ft arg2}
            | isPrefixOf "insert left" s = do {return $ insertLeft ft arg2}
            | isPrefixOf "insert right" s = do {return $ insertRight ft arg2}
            | isPrefixOf "delete" s = do {return $ del ft}
            | otherwise = error "UNCK COMMAND!"
        where
            arg1 = read $ ((words s) !! 1)
            arg2 = read $ ((words s) !! 2)
            
act :: FocusTree Int ->  IO (FocusTree Int)
act ft = do 
           s <- getLine            
           handle ft s

ft = (Node 0 [],[]) :: FocusTree Int
ft1 = (Node 0 [Node 1 []],[1])

stopc :: (Int,FocusTree Int) -> Bool
stopc (n,_) = if n > 0 then False else True

main = do 
        n <- (readLn::IO Int)
        iterateUntilM stopc (\(n,ft) -> ((,) (n-1)) <$> act ft ) (n,ft)
        return ()
