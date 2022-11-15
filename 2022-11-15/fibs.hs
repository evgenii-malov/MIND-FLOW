-- fibs list

f n = go 0 [0]
    where
        go 0 _ = go 1 [0]
        go 1 _ = go 2 [1,0]
        go i (a:b:g) | i<n = go (i+1) ((a+b):a:b:g)
                     | i == n = reverse $ ((a+b):a:b:g)

