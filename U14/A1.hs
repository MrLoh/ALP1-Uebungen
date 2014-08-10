list1 = iterate (\b -> if b then False else True) True
list2 = iterate (\i -> (-1)*(i*2)) 2
list3 = map prodTupl (iterate (\p -> (snd p, 1+snd p) ) (1,2))
        where prodTupl (x,y) = x*y


main = do
  print( take 10 list1 )
  print( take 10 list2 )
  print( take 10 list3 )
