import Data.Function

list1 = iterate (\b -> if b then False else True) True
list2 = iterate (\i -> (-1)*(i*2)) 2
list3 = fix (\r ((x,y):xs) -> (x*y):(r xs) ) (iterate (\p -> (snd p, 1+snd p) ) (1,2))


main = do
  print( take 10 list1 )
  print( take 10 list2 )
  print( take 10 list3 )
