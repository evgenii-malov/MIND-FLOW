{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Main where

class TypeOf a where
  typeOf :: String

instance TypeOf Bool where
  typeOf = "Bool"

instance TypeOf Char where
  typeOf = "Char"  

instance TypeOf Int where
  typeOf = "Int"  

instance TypeOf [a] where
  typeOf = "List"  

guardUnit :: forall a. TypeOf a => a -> Either String a
guardUnit x = case typeOf @a of
  "Bool"  -> Left "Bool"
  "Char" -> Right x
  "Int" -> Left "Int"

guardUnit1 :: forall a. TypeOf a => a -> Either String a
guardUnit1 x 
  | typeOf @a == "Bool" = Left "Bool"
  | typeOf @a == "Char" = Right x
  | typeOf @a == "Int" = Left "Int"  
  | otherwise = Right x
  


-- tweakId :: forall a. TypeOf a => a -> a
-- tweakId x = case typeOf @a of
--    "Bool" -> not x
--    "Int" -> x+1
--    "Char" -> x

tweakId :: forall a. TypeOf a => a -> a
tweakId x 
  | typeOf @a == "Bool" = not x
  | typeOf @a == "Int" = x+1
  | typeOf @a == "Char" = x
  | otherwise = x

-- https://stackoverflow.com/questions/75372787/can-we-tweak-a-a-function-in-haskell?noredirect=1#comment132999705_75372787
-- https://kseo.github.io/posts/2017-01-08-visible-type-application-ghc8.html
-- https://www.haskellforall.com/2015/10/polymorphism-for-dummies.html
-- https://en.wikipedia.org/wiki/System_F
-- https://stackoverflow.com/questions/17380379/where-do-values-fit-in-category-of-hask/21485660#21485660
-- https://stackoverflow.com/questions/10811657/why-is-function-definition-for-all-types-at-once-not-allowed-in-haskell?rq=1    
-- https://en.wikipedia.org/wiki/Parametricity
-- https://www.well-typed.com/blog/2015/05/parametricity/
-- https://stackoverflow.com/questions/54452440/regarding-haskells-parametricity-concept
-- https://stackoverflow.com/questions/75372787/can-we-tweak-a-a-function-in-haskell?noredirect=1#comment132999705_75372787


-- https://en.wikipedia.org/wiki/Parametric_polymorphism
-- https://stackoverflow.com/questions/54452440/regarding-haskells-parametricity-concept
-- https://www.reddit.com/r/haskellquestions/comments/6fkufo/free_theorems/
-- https://stackoverflow.com/questions/31122882/what-is-the-category-theoretical-basis-for-the-requirement-that-the-haskell-id
-- https://stackoverflow.com/questions/17380379/where-do-values-fit-in-category-of-hask/21485660#21485660
-- https://stackoverflow.com/questions/54452440/regarding-haskells-parametricity-concept

main :: IO ()
main = putStrLn "Hello, Haskell!"
