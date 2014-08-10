import Data.Function

anonymFak :: Integral a => a -> a
anonymFak = fix(\f n -> if n==0 then 1 else n*(f(n-1)))

--{ AUFGABE 1 }--
list1 = iterate (\b -> if b then False else True) True

list2 = iterate (\i -> (-1)*(i*2)) 2

list3 = iterate (\x -> (fix(\s l i -> if i==(head l) then head(tail l) else (s (tail l) i))) [n*(n+1)|n <- [1..]] x) 2
-- list3 = iterate (\x -> (fix(\s l i -> if i==(head l) then head(tail l) 
                 else (s (tail l) i))) [n*(n+1)|n <- [1..]] x) 2
                 

-- A1 Reste:

-- succL :: Eq a => a -> [a] -> a
-- succL e [x] = error "Successor not found"
-- succL e (x:xs)
--          | e==x = head xs
--          | otherwise = succL e xs
-- 
-- helpL = [x*(x+1)|x <- [1..]]
-- 
-- succL :: Integral a => [a] -> a -> a
-- succL = fix(\s l i -> if i==(head l) then head(tail l) else (s (tail l) i))
-- 
-- succL' :: Integral a => a -> a
-- succL' = (\x -> (fix(\s l i -> if i==(head l) then head(tail l) else (s (tail l) i))) [n*(n+1)|n <- [1..]] x)



--{ AUFGABE 2 }--

unfold p f g x 
       | p x = []
       | otherwise = f x : unfold p f g (g x)

-- Zur Referenz: dec2bin Funktion aus V5_ALP1_Rec_Listen_SoSe_2014.pdf 
dec2bin :: Int -> [Int]
dec2bin n 
        | n<2 = [n]
        | otherwise = dec2bin (n`div`2) ++ [n `mod`2]



iterate' f = unfold (\x -> False) (\x -> x) (\y -> f y)
map' f = unfold (\xs -> xs == []) (\(x:xs) -> f x) (\(x:xs) -> xs)
int2bin n = rev (unfold (\n -> n==0) (\n -> mod n 2) (\n -> div n 2) n ) []
           where
             -- rev mit unfold möglich, aber unschön
             -- rev = unfold (\xs -> xs == []) (\xs -> last xs) (\xs -> init xs)
             rev []     ys = ys
             rev (x:xs) ys = rev xs (x:ys)


--{ AUFGABE 3 }--

reverse' :: Eq a => [a] -> [a]
reverse' xs = if xs == []
                     then []
                     else reverse' (tail xs) ++ [(head xs)]

(+++) :: Eq a => [a] -> [a] -> [a]
(+++) xs ys = if xs == []
             then ys
             else (head xs):((+++) (tail xs) ys)
