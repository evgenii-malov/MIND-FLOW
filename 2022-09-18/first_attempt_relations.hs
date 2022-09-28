{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}
import Data.List (elem)
-- binary relation defined by a set of ordered pairs
-- A binary relation over sets X and Y is an element of the power set of X × Y. 
-- relation is either a homogeneous (or endorelation) relation or a heterogeneous relation 
-- depending on whether X = Y or not.


-- domain: set which contains all first elements of relation
-- range: set which contains all second elements of relation

-- Range ⊆ Codomain

-- data HRel a = [(a,a)] 

-- r1 = [(1,2),(1,1),(2,2),(3,2)]

-- -- for all x ∈ X, xRx
-- reflexive :: Eq a => Hrel a -> Bool

-- homogeneous relation (or endorelation) over a set X 
-- is a binary relation over X and itself, 
-- i.e. it is a subset of the Cartesian product X × X.
--  This is commonly phrased as "a relation on X" or "a (binary) relation over X"

data RelL a s = RelL (s a) [(a,a)] deriving Show
r1 = RelL [1,2,3,4,5] [(1,2),(3,4)]

class Hrel rel s | rel -> s where
--   reflexive :: (CSet s,CSet (rel a s)) => rel a s -> Bool
  set :: rel -> s
  --reflexive :: rel -> Bool

instance Hrel (RelL a []) [a] where
  set (RelL l _) = l



class Eq a => Set set a | set -> a where
  empty :: set 
  insert :: a -> set -> set
  member :: a -> set -> Bool

instance (Eq a) => Set [a] a where
  empty = []
  insert e l = if member e l then l else e:l
  member = elem
  



-- reflexive :: (Cset set a), (CHrel rel a) => set a -> rel a -> Bool

-- https://stackoverflow.com/questions/44243367/rigid-type-variable-in-haskell
-- https://stackoverflow.com/questions/4629883/couldnt-match-expected-type-against-inferred-type-rigid-type-variable-error
-- https://www.slideshare.net/HeejongAhn/just-enough-category-theory-for-haskell-part-1
-- https://degoes.net/articles/when-to-typeclass#:~:text=To%20add%20structure%20to%20a,values%20of%20some%20unknown%20type.
-- https://stackoverflow.com/questions/34790721/where-is-the-set-type-class
-- https://stackoverflow.com/questions/3623612/in-haskell-why-isnt-there-a-typeclass-for-things-that-can-act-like-lists/8484117#8484117
