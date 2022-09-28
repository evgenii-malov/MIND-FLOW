{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
-- binary relation defined by a set of ordered pairs
-- A binary relation over sets X and Y is an element of the power set of X × Y. 
-- relation is either a homogeneous (or endorelation) relation or a heterogeneous relation 
-- depending on whether X = Y or not.


-- domain: set which contains all first elements of relation
-- range: set which contains all second elements of relation

-- Range ⊆ Codomain

-- homogeneous relation (or endorelation) over a set X 
-- is a binary relation over X and itself, 
-- i.e. it is a subset of the Cartesian product X × X.
--  This is commonly phrased as "a relation on X" or "a (binary) relation over X"

-- type HRel a = [(a,a)]
-- newtype HRel a = Hrel [(a,a)] deriving Foldable
data HRel a = Hrel [(a,a)] deriving (Foldable, Functor)

c :: HRel a -> Int
c r = length r

-- reflexive :: (Foldable hr,Foldable s, Functor hr, Functor s, Eq a) => s a -> hr (a,a) -> Bool
-- reflexive s r = all id $ (\e -> elem (e,e) r) <$> s

symmetric :: (Foldable hr, Functor hr, Eq a) => hr a -> Bool
symmetric r = all id $ (\(a,b) -> elem (b,a) r) <$> r
