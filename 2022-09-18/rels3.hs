import qualified Data.Set as S
import Control.Applicative
import Debug.Trace
{-
Given sets X and Y, the Cartesian product X × Y is defined as {(x, y) | x ∈ X and y ∈ Y}
elements are called ordered pairs.
binary relation - ordered triple (X, Y, G), where G is a subset of X × Y called the graph of the binary relation
X - domain or set of departure of R, 
Y - codomain or set of destination of R.  
if X = Y => homogeneous relation (or endorelation) else heterogeneous relation.
The domain of definition or active domain of R - set of all x such that xRy 
active domain ⊆ domain
image or range of R (or codomain of definition, active codomain) - set of all y such that xRy
Range ⊆ Codomain
homogeneous relation is phrased as "a relation on X" or "a (binary) relation over X"  

Binary relation defined by a set of ordered pairs
A binary relation over sets X and Y is an element of the power set of X × Y. 
-}

data Rel a b = Rel { x :: S.Set a, y :: S.Set b , g :: S.Set (a,b) } deriving Show

image :: Ord b => Rel a b -> S.Set b
image (Rel _ _ g) = S.fromList [b | (_,b) <- S.toList g]

active_domain :: Ord a => Rel a b -> S.Set a
active_domain (Rel _ _ g) = S.fromList [a | (a,_) <- S.toList g]

fromList :: (Ord a, Ord b, Show a, Show b) => [a] -> [b] -> [(a,b)] -> Rel a b
fromList x y g = if null not_in_y && null not_in_x then Rel (S.fromList x) (S.fromList y) (S.fromList g) 
                  else error $ "elements not in X:" ++ show not_in_x ++ " elements not in Y: " ++ show not_in_y
                  where 
                    not_in_x = [a | (a,_) <- g, not $ elem a x]
                    not_in_y = [b | (_,b) <- g, not $ elem b y]  

-- Some important particular homogeneous relations over a set X are:
-- The empty relation
-- E = ∅ ⊆ X × X;
-- The universal relation
-- U = X × X;
-- The identity relation
-- I = {(x, x) | x ∈ X}.                    

empty ::  Ord a => [a] -> Rel a a
empty x = Rel (S.fromList x) (S.fromList x) (S.empty)

universal :: Ord a => [a] -> Rel a a
universal xs = Rel (S.fromList xs) (S.fromList xs) (S.fromList $ liftA2 (,) xs xs)

identity :: Ord a => [a] -> Rel a a
identity xs = Rel (S.fromList xs) (S.fromList xs) (S.fromList [(x,x) | x <- xs]) 


-- for all x ∈ X, xRx
reflexive :: Ord a => Rel a a -> Bool
reflexive (Rel xs _ gs) = all (\x -> S.member (x,x) gs) xs
-- foldl (\s x -> s && S.member (x,x) gs) True xs

-- Irreflexive (or strict): for all x ∈ X, not xRx. 
irreflexive :: Ord a => Rel a a -> Bool
irreflexive (Rel xs _ gs) = all (\x -> not $ S.member (x,x) gs) xs

-- reflexive and irreflexive are not exhaustive;  reflexive != not irreflexive

--for all x, y ∈ X, if xRy then yRx
symmetric :: Ord a => Rel a a -> Bool
symmetric (Rel _ _ gs) = all (\(a,b) -> S.member (b,a) gs) gs

-- antisymmetric -- for all x, y ∈ X, if xRy and yRx then x = y. 
antisymmetric :: Ord a => Rel a a -> Bool
antisymmetric (Rel _ _ gs) = all (\(a,b) -> a == b || (not $ S.member (b,a) gs)) gs

-- asymmetric -- for all x, y ∈ X, if xRy then not yRx
-- A relation is asymmetric if and only if it is both antisymmetric and irreflexive
asymmetric  :: Ord a => Rel a a -> Bool
asymmetric (Rel _ _ gs) = all (\(a,b) -> not $ S.member (b,a) gs) gs

 -- symmetric, antisymmetric, asymmetric are far from being exhaustive

 -- for all x, y, z ∈ X, if xRy and yRz then xRz. A transitive relation is irreflexive if and only if it is asymmetric
transitive  :: (Ord a) => Rel a a -> Bool
transitive (Rel _ _ gs) = all (\(x,y) -> all (\(_,z) -> S.member (x,z) gs) $ relfromY y) gs
                           where
                              relfromY y = S.filter (\(x,_) -> x == y) gs
-- transitive $ fromList [1,2,3] [1,2,3] [(1,2),(2,1),(1,1),(2,2)]


-- Connected - for all x, y ∈ X, if x ≠ y then xRy or yRx. 
connected :: Ord a => Rel a a -> Bool
connected (Rel x _ gs) = all (\(a,b) -> a == b || elem (a,b) gs || elem (b,a) gs) (S.cartesianProduct x x) 


-- Strongly connected - for all x, y ∈ X, xRy or yRx.
strongly_connected :: Ord a => Rel a a -> Bool
strongly_connected (Rel x _ gs) = all (\(a,b) -> elem (a,b) gs || elem (b,a) gs) (S.cartesianProduct x x) 

-- Trichotomous for all x, y ∈ X, exactly one of xRy, yRx or x = y holds.
-- A relation is trichotomous if, and only if, it is asymmetric and connected.  
-- https://en.wikipedia.org/wiki/Law_of_trichotomy  
trichotomous :: Ord a => Rel a a -> Bool
trichotomous (Rel x _ gs) = all (\(a,b) -> c a b) (S.cartesianProduct x x) 
              where c a b = a == b && (elem (a,b) gs) ||
                            a /= b && (elem (a,b) gs) && (not $ elem (b,a) gs) || 
                            a /= b && (not $ elem (a,b) gs) && (elem (b,a) gs)


-- Serial (or left-total) for all x ∈ X, there exists some y ∈ X such that xRy.
-- Every reflexive relation is serial: for a given x, choose y = x.        
serial :: Ord a => Rel a a -> Bool
serial (Rel x _ gs) = all (\a -> any (\(a',_)->a'==a) gs ) x

-- Dense
-- for all x, y ∈ X such that xRy, there exists some z ∈ X such that xRz and zRy. This is used in dense orders.

-- Well-founded
-- every nonempty subset S of X contains a minimal element with respect to R. 

-- Preorder
-- A relation that is reflexive and transitive.

preorder :: Ord a => Rel a a -> Bool
preorder r = reflexive r && transitive r

-- Total preorder (also, linear preorder or weak order)
-- A relation that is reflexive, transitive, and connected.
total_preorder :: Ord a => Rel a a -> Bool
total_preorder r = preorder r && connected r

-- Partial order 
-- A relation that is reflexive, antisymmetric, and transitive.
partial_order :: Ord a => Rel a a -> Bool
partial_order r = reflexive r && antisymmetric r && transitive r

-- Strict partial order 
-- A relation that is irreflexive, antisymmetric, and transitive.
strict_partial_order :: Ord a => Rel a a -> Bool
strict_partial_order r = irreflexive r && antisymmetric r && transitive r


-- Total order (also, linear order, simple order, or chain)
-- A relation that is reflexive, antisymmetric, transitive and connected.
total_order :: Ord a => Rel a a -> Bool
total_order r = reflexive r && antisymmetric r && transitive r && connected r

-- Strict total order (also, strict linear order, strict simple order, or strict chain)
-- A relation that is irreflexive, antisymmetric, transitive and connected.
strict_total_order :: Ord a => Rel a a -> Bool
strict_total_order r = irreflexive r && antisymmetric r && transitive r && connected r

-- Partial equivalence relation
-- A relation that is symmetric and transitive.
partial_equivalence_relation :: Ord a => Rel a a -> Bool
partial_equivalence_relation r = symmetric r && transitive r

-- Equivalence relation
-- A relation that is reflexive, symmetric, and transitive. 
-- It is also a relation that is symmetric, transitive, and serial, since these properties imply reflexivity.
equivalence_relation :: Ord a => Rel a a -> Bool
equivalence_relation r = reflexive r && symmetric r && transitive r

-- homogeneous relation or a heterogeneous relation properties:
-- Uniqueness properties:

-- Injective (also called left-unique)
-- For all x, z ∈ X and all y ∈ Y, if xRy and zRy then x = z. 
-- For such a relation, {Y} is called a primary key of R.

rel_to_y :: (Ord a, Ord b) => Rel a b -> b -> S.Set a
rel_to_y (Rel xs ys g) y = S.map (\(x,_)->x) (S.filter (\(_,y')-> y==y') g)

injective :: (Ord a, Ord b) => Rel a b -> Bool
injective r@(Rel _ _ _) = all (\y -> length (rel_to_y r y) == 1) $ image r

-- Functional (also called right-unique,[15]right-definite[16] or univalent)
-- For all x ∈ X and all y, z ∈ Y, if xRy and xRz then y = z. 
-- Such a binary relation is called a partial function. 
-- For such a relation, {X} is called a primary key of R.

rel_to_x :: (Ord a, Ord b) => Rel a b -> a -> S.Set b
rel_to_x (Rel xs ys g) x = S.map (\(_,y)->y) (S.filter (\(x',_)-> x==x') g)

functional :: (Ord a, Ord b) => Rel a b -> Bool
functional r@(Rel _ _ _) = all (\x -> length (rel_to_x r x) == 1) $ active_domain r

-- One-to-one
-- Injective and functional.

onetoone :: (Ord a, Ord b) => Rel a b -> Bool
onetoone r = injective r && functional r

-- One-to-many
-- Injective and not functional. 

onetomany :: (Ord a, Ord b) => Rel a b -> Bool
onetomany r = injective r && not (functional r)

-- Many-to-one
-- Functional and not injective.

manytoone :: (Ord a, Ord b) => Rel a b -> Bool
manytoone r = not (injective r) && functional r

-- Many-to-many
-- Not injective nor functional
manytomany :: (Ord a, Ord b) => Rel a b -> Bool
manytomany r = not (injective r) && not (functional r)


-- Totality properties

-- Total (also called left-total)
-- For all x in X there exists a y in Y such that xRy.
total :: (Ord a, Ord b) => Rel a b -> Bool
total r@(Rel xs _ _) = (active_domain r) == xs

-- Surjective (also called right-total[15] or onto)
-- For all y in Y, there exists an x in X such that xRy.
surjective :: (Ord a, Ord b) => Rel a b -> Bool
surjective r@(Rel _ ys _) = (image r) == ys

-- A binary relation that is functional and total. 
function :: (Ord a, Ord b) => Rel a b -> Bool
function r = (functional r) && (total r)

-- An injection
-- A function that is injective.
injection :: (Ord a, Ord b) => Rel a b -> Bool
injection r = (function r) && (injective r)

-- A surjection
-- A function that is surjective
surjection :: (Ord a, Ord b) => Rel a b -> Bool
surjection r = (function r) && (surjective r)


-- A bijection
--A function that is injective and surjective.
bijection :: (Ord a, Ord b) => Rel a b -> Bool
bijection r = (function r) &&  (injective r) && (surjective r)
