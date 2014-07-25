

elementOf :: Int -> Set -> Bool
elementOf e []       = False
elementOf e (el:set) | e == el   = True
                     | otherwise = elementOf e set

gleich :: Set -> Set -> Bool
gleich [] []                 = True
gleich (el1:set1) (el2:set2) | el1 == el2 = gleich set1 set2
                             | otherwise  = False
gleich _ _                   = False

teilmenge :: Set -> Set -> Bool
teilmenge []        set2 = True
teilmenge (el:set1) set2 | el `elementOf` set2 = teilmenge set1 set2
                         | otherwise           = False

vereinigung :: Set -> Set -> Set
vereinigung set1 set2 = mks (set1 ++ set2)
                        where
                        mks :: Set -> Set
                        mks []     = []
                        mks (x:xs) | (elem x xs) = mks xs
                                   | otherwise   = x : (mks xs)

schnittmenge :: Set -> Set -> Set
schnittmenge []   set2      = []
schnittmenge set1 []        = []
schnittmenge (el:set1) set2 | el `elementOf` set2 = el:(schnittmenge set1 set2)
                            | otherwise           = schnittmenge set1 set2

mengendifferenz :: Set -> Set -> Set
mengendifferenz []   set2      = set2
mengendifferenz set1 set2 = dff set1 set2
                            where
                            dff []        set2 = []
                            dff (el:set1) set2 | el `elementOf` set2 = dff set1 set2
                                               | otherwise           = el:(dff set1 set2)
