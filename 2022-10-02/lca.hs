{-# LANGUAGE OverloadedStrings #-}
import Data.Text
import qualified Data.Text.IO as TIO (putStr)
import Data.ByteString
-- Lowest Common Ancestor of a Binary Tree

-- data LcaR a = No | OneIn a | R a
-- data Btree a = Empty | Node (Btree a) a (Btree a)

-- lca :: Btree a -> a -> a -> LcaR a
-- lca t a b = lca' t
--             where 
--               lca' Empty = No
--               lca' (Node l e r) | one_left && one_right = R e
--                                 | no_left = lca' r
--                                 | no_right = lca' l
                                

a :: Text
a = "Привет мир ! Hello World"

b :: ByteString
b = "Привет Мир! Hello World"