{-# LANGUAGE ExistentialQuantification #-}
--
-- An existential type encapsulating types that can be Shown
-- The interface to the type is held in the show method dictionary
--
-- Create your own typeclass for packing up other interfaces
--
data Showable = forall a . Show a => MkShowable a

--
-- And a nice existential builder
--
pack :: Show a => a -> Showable
pack = MkShowable

--
-- A heteoregenous list of Showable values
--
hlist :: [Showable]
hlist = [ MkShowable 3
        , pack 'x'
        , pack pi
        , pack "string"
        , pack (Just ()) ]

--
-- The only thing we can do to Showable values is show them
--
main :: IO ()
main = print $ map f hlist
    where
        f (MkShowable a) = show a

{-

*Main> main
["3","'x'","3.141592653589793","\"string\"","Just ()"]

-}
