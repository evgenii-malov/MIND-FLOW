import qualified Data.Set as S
import Control.Applicative

--- Given sets X and Y, the Cartesian product X × Y is defined as {(x, y) | x ∈ X and y ∈ Y}
-- elements are called ordered pairs.
-- X - domain or set of departure of R, 
-- Y - codomain or set of destination of R.  
-- binary relation - ordered triple (X, Y, G), where G is a subset of X × Y called the graph of the binary relation
-- if X = Y => homogeneous relation (or endorelation) else heterogeneous relation.
-- The domain of definition or active domain of R - set of all x such that xRy 
-- image or range of R (or codomain of definition, active codomain) - set of all y such that xRy
-- Range ⊆ Codomain
-- homogeneous relation is phrased as "a relation on X" or "a (binary) relation over X"  

-- binary relation defined by a set of ordered pairs
-- A binary relation over sets X and Y is an element of the power set of X × Y. 


data Hrel a = Hrel {set :: S.Set a, rel:: S.Set (a,a)} deriving Show

range :: Hrel a -> S.Set a
range = undefined

active_domain :: Hrel a -> S.Set a
active_domain = undefined

fromList :: (Ord a,Show a) => [a] -> [(a,a)] -> Hrel a
--fromList s r = if all id $ (\(a,b) -> elem a s && elem b s) <$> r then Hrel (S.fromList s) (S.fromList r) else error "inconsitent lists"
fromList s r = if null not_in_set then Hrel (S.fromList s) (S.fromList r) else error $ "relation elems not in set:" ++ show not_in_set
                where
                  not_in_set = [a | (a,_) <- r, not $ elem a s] ++ [b | (_,b) <- r, not $ elem b s]

r = fromList [1,2,3] [(1,2),(2,3),(1,3)]

reflexive :: Eq a => Hrel a -> Bool
reflexive (Hrel s r) = all id $ (\e -> elem (e,e) r) <$> (S.toList s)

symmetric :: Ord a => Hrel a -> Bool
symmetric (Hrel s r) = all id $ (\(a,b) -> S.member (b,a) r) <$> (S.toList r)

transitive :: Ord a => Hrel a -> Bool
transitive (Hrel s r) = all id $ ((\(a,b)-> all id $ (\(b,c) -> S.member (a,c) r) <$> all_from b ) <$> lr)
            where all_from b = [(b',c) | (b',c) <- lr, b==b']
                  lr = S.toList r
