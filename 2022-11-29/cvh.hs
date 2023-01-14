solve :: [(Int, Int)] -> Double
solve points = sum $ zip (\(x1,y1) (x2,y2) -> sqrt((x1-x2)^2+(y1-y2)^2)) convex_h $ tail convex_h
    where
        convex_h = go init_stack candidates
        go st [] = st
        go st@(s1:s2:ts) cds@(c3:cdt)
            | negative = go (c3:st) cdt
            | otherwise = go (s2:ts) cds
            where
                negative = _ -- z component of croos pr <= 0
        candidates = o_points // init_stack
        init_stack = (head o_points):[(head $ tail o_points)]        
        o_points = _
        
