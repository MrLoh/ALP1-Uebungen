smap :: Int -> Int -> (Int -> Double) -> Double
smap s n func = sum (map func [s..n])

bla :: Int -> Double
bla k = 4.0*(-1.0)^k/(2.0*(fromIntegral k)+1.0)

piappr :: Int -> Double
piappr n = smap 0 n bla

main = do
 print( piappr 300 )
