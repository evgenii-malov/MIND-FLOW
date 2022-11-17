import Debug.Trace
import Data.List 
import Data.Maybe

pf :: String -> String
pf [] = trace ("empty list") ""
pf (e:[]) = trace ("one list") ""
pf xs = if (xs !! length p_) == last xs then trace ("xs:"++xs++" p_:"++p_) p_ ++ [last xs] else ""
        where
        p_ = fromJust (find (\p -> (xs !! length p == last xs) || (length p) == 0 ) $ iterate pf $ trace "init" (init xs)) 
      
