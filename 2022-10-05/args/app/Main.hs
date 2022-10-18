{-# LANGUAGE ExistentialQuantification #-}
module Main where
import GHC.IO.Exception (IOErrorType(UserError))



newtype Email = Email String deriving (Show)
newtype User = User String deriving (Show)

data EmailError = MustNotBeEmpty
            | MustContainAt
            | MustContainPeriod
            deriving (Show)

data UserError =
  ToBigUserLength Int Int |
  ToSmallUserLength Int Int deriving Show
--  InvalidUserChars String String deriving Show

class FJ a where
  fj :: String -> a

class TJ a where
  tj :: a -> String

instance FJ Email where
  fj = undefined  

instance FJ User where
  fj = undefined

instance TJ UserError where
  tj = undefined

instance TJ EmailError where
  tj = undefined


--data Arg = forall a e.(FJ a, TJ e) => Arg a | Errors [e]
data Arg = forall a .(FJ a) => Arg a | forall e.(TJ e) => Errors [e] 
--data Args = forall a e.(FJ a, TJ e)  => Args [Arg a e]

args = [("email", Arg $ Email "e1@m.ru"), 
        ("user", Arg $ User "u1"), 
        ("email2", Errors [MustContainAt])]


data RegArgs = RegArgs {
  username :: Arg,
  email :: Arg
}     

main :: IO ()
main = putStrLn "Hello, Haskell!"
