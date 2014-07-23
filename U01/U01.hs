import Data.Char

--Aufgabe 1
teil :: Integer -> Integer -> Bool
teil a b = a `mod` b == 0

--Aufgabe 2
ungerade :: Integer -> Bool
ungerade n = rem n 2 /= 0

--Aufgabe 3
pythTripel :: (Int,Int,Int) -> Bool
pythTripel (a,b,c) = let sqa = a*a
                         sqb = b*b
                         sqc = c*c
                     in sqa + sqb == sqc || sqa + sqc == sqb || sqb + sqc == sqa

--pythTripel :: Int -> Int -> Int -> Bool
--pythTripel a b c = a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2

--Aufgabe 4
mehrfaches :: Int -> Int -> Int -> Bool
mehrfaches a b c = a*b == c || a*c == b || b*c == a
--mehrfaches a b c = (teil a b && teil teil a c) || (teil b c && teil b a) || (teil a c && teil c b)


--Aufgabe 5
myToLower :: Char -> Char
--toLower c = let n = fromEnum c in if n >= 63 then toEnum (n+32) :: Char else c
myToLower c | isUpper c = toEnum(fromEnum c + 32)::Char
            | otherwise = c
            where
                isUpper c = c >= 'A' && c <= 'Z'

--Aufgabe 6
isKlammer :: Char -> Bool
isKlammer c = elem c "(){}[]<>"

--Aufgabe 7
weekday :: Int -> Int -> Int -> String
weekday d m y = case (name) of
                     0 -> "Su"
                     1 -> "Mo"
                     2 -> "Di"
                     3 -> "Mi"
                     4 -> "Do"
                     5 -> "Fr"
                     6 -> "Sa"
                where
                    y0 = y - (14-m)`div`12
                    x = y0+y0`div`4-y0`div`100+y0`div`400
                    m0 = m+12*(14-m)`div`12-2
                    name = (d+x+(31*m0)`div`12) `mod` 7

--Aufgabe 8
polygonArea :: Double -> Double -> Double -> Double
polygonArea n s a = n*s*a/2
             where
                a = s/2*tan(pi/n)

--Aufgabe 9
type Point = (Double, Double)
type Rectangle = (Point, Point)
area :: Rectangle -> Double
area ((p1x,p1y),(p2x,p2y)) = abs(p1x-p2x)*abs(p1y-p2y)

inside :: Point -> Rectangle -> Bool
inside (x,y) ((p1x,p1y),(p2x,p2y)) = let ox = max(p1x,p2x)
                                         ux = min(p1x,p2x)
                                         oy = max(p1y,p2y)
                                         uy = min(p1y,p2y)
                                     in (ox >= x) && (ux <= x) && (oy >= y) && (uy <= y)

overlaps :: Rectangle -> Rectangle -> Bool
overlaps ((q1x,q1y),(q2x,q2y)) ((p1x,p1y),(p2x,p2y)) = let pox = max(p1x,p2x)
                                                           pux = min(p1x,p2x)
                                                           poy = max(p1y,p2y)
                                                           puy = min(p1y,p2y)
                                                           qox = max(q1x,q2x)
                                                           qux = min(q1x,q2x)
                                                           qoy = max(q1y,q2y)
                                                           quy = min(q1y,q2y)
                                                       in (qux >= pox) || (pux >= qox) || (quy >= poy) || (puy >= qoy)
contains :: Rectangle -> Rectangle -> Bool
contains (q1,q2) (p1,p2) = inside p1 (q1,q2) && inside p1 (q1,q2) || inside q1 (p1,p2) && inside q1 (p1,p2)

--overlaps :: Rectangle -> Rectangle -> Bool
--overlaps ((r1p1x,r1p1y),(r1p2x,r1p2y)) ((r2p1x,r2p1y),(r2p2x,r2p2y)) =





