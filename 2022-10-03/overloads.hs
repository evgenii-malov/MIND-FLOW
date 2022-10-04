{-# LANGUAGE OverloadedStrings #-}

-- A time and space-efficient implementation of Unicode text. 
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
-- A time- and space-efficient implementation of byte vectors using packed Word8 arrays
-- For binary data, or data of chars in ASCII range, ByteString can be used.


-- Strings of Char are linked lists to slow and require more memory

-- https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.String.html#IsString
-- https://riptutorial.com/haskell/example/4173/overloadedstrings
-- https://stackoverflow.com/questions/47446588/difference-between-data-bytestring-and-data-bytestring-char8

-- class IsString a where
--     fromString :: String -> a

s = "simple string"

ts = "text string" :: T.Text
bs = "ascii byte string" :: BS.ByteString

data Foo = A | B | Other String deriving Show

instance S.IsString Foo where
  fromString "A" = A
  fromString "B" = B
  fromString xs = Other xs

mt = "A" :: Foo

enc = E.encodeUtf8 ts
