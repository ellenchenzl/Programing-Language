module HW3 where


-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
-- 
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--
expandlist :: Eq a => a -> [(Int, a)] -> [(Int, a)]
expandlist (c) ((n, b):ys) = if c == b then (n + 1, c):ys else (1, c):(n, b):ys

compress :: Eq a => [a] -> [(Int,a)]
compress ([c]) = [(1,c)]
compress (b:ys)= expandlist b (compress ys)

-- addtolist :: Eq a => a -> [(Int, a)] -> [(Int, a)]
-- addtolist (x) ((n, y):ys) = if x == y then (n + 1, x):ys else (1, x):(n, y):ys

-- compress :: Eq a => [a] -> [(Int,a)]
-- compress ([x]) = [(1, x)]
-- compress (x:xs) = addtolist x (compress xs)

-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--  
decompress :: [(Int,a)] -> [a]
decompress [(0,_)]   = []
decompress ((n,b):xs) = if (n<=0) then (decompress(xs))
                        else (b: (decompress (((n-1),b):xs) ))

