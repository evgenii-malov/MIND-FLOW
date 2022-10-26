{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-} -- for aeson encode
module Main where
import           Control.Exception
import Control.Lens hiding ((.=)) 
import           Control.Monad
import           Data.Aeson 
import qualified Data.Aeson.Key as LB
import           Data.Aeson.Types

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import           Data.Either (isLeft, fromLeft, fromRight)
import qualified Data.Either.Combinators as EithQ

import           Data.Maybe
import qualified Data.Text as T
import GHC.Generics

import qualified Data.Text.IO as TIO
import           Data.Text.Lazy.Encoding



import           Control.Monad      (forever)
import qualified Network.WebSockets as WS
import Core 
import BaseT
import BaseTJson
import Users
import UsersJson


jsonStringAct :: LB.ByteString
jsonStringAct = "{ \"action\": \"reg\", \"username\": \"John\" , \"email\": \"john@gmail.com\" }"
jsonStringAct2 = "{ \"action\": \"reg\", \"username\": \"John\" , \"email\": \"john!gmail.com\" }":: LB.ByteString


re = RegArgsErrors{username_error=[],email_error=FieldRequired}
re2 = RegArgsErrors{username_error=[],email_error=FieldErrors [MustContainAt,MustNotBeEmpty]}

     

-- !!! catch parse error !!!


router :: LB.ByteString -> IO LB.ByteString
router b = case act of "reg" -> router_ (parseCmdfromJson b) reg
                       _ -> return $ "uncknown act " `mappend` LBU8.fromString act
           where (act :: String) = fromJust $ exF b "action"




chandle :: WS.Connection -> IO ()
chandle conn = forever $ do
    msg <- WS.receiveData conn
    resp <- router msg
    --WS.sendTextData conn $ msg `T.append` ", handle"
    WS.sendTextData conn resp
    

main = print "serving 8080" >> (WS.runServer "127.0.0.1" 8080 $ \pending -> do
       conn <- WS.acceptRequest pending    
       print "accept conn"
       chandle conn)

-- ws://127.0.0.1:8080
-- { "action": "reg", "username": "John" , "email": "john@gmail.com" }       
-- https://codereview.stackexchange.com/questions/200310/using-mysql-with-wai-and-warp
-- https://hackage.haskell.org/package/resource-pool-0.3.1.0/docs/Data-Pool.html    

-- https://begriffs.com/posts/2014-10-25-creating-package-hackage.html
-- https://cabal.readthedocs.io/en/3.4/developing-packages.html