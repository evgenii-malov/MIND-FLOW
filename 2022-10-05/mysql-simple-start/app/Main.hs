{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Control.Monad

-- https://stackoverflow.com/questions/1141677/concurrent-db-connection-pool-in-haskell

data User = User { firstName :: String, lastName :: String }

-- instance QueryResults User where
--     convertResults [fa,fb] [va,vb] = User <$> a <*> b
--         where !a = convert fa va
--               !b = convert fb vb
--     convertResults fs vs  = convertError fs vs 2

main :: IO ()
main = do 
        i <- q2
        print i
        putStrLn "Hello, Haskell!" 

q1 :: IO Int
q1 = do
  conn <- connect defaultConnectInfo { connectPassword = "root", connectDatabase = "test" }
  [Only i] <- query_ conn "select 2 + 2"
  return i

q2 = do 
      conn <- connect defaultConnectInfo { connectPassword = "root", connectDatabase = "test" }
      xs :: [(Int,Text.Text,Int)] <- query_ conn "select id,name,age from users"
      forM_ xs $ \(id,name,age) ->
        putStrLn $ show id ++ Text.unpack name ++ " is " ++ show age
