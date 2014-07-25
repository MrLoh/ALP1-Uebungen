minNeighborsDistance :: [Int] -> Int
minNeighborsDistance (x:y:xs) = fst (mnd (abs (x-y) ,xs))
                                where
                                mnd :: (Int,[Int]) -> (Int,[Int])
                                mnd (m,[])       = (m,[])
                                mnd (m,[x])      = (m,[x])
                                mnd (m,(x:y:xs)) | dist<m   = mnd (dist,(y:xs))
                                                 | otherwise = mnd (m,(y:xs))
                                                   where dist = abs (x-y)
minNeighborsDistance _        = error "list needs at least 2 elements"

main = do
 print( minNeighborsDistance [2,3,6,2,0,1,9,8] )
 print( minNeighborsDistance [2,5,6,1,9] )
