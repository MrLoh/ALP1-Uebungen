import PRF

-- A1
expp :: PRFunction
expp = comp exppH [(p 2), (p 1)]
       where
       exppH = pr exppH (const 1) (comp mul [p 3,p 1])

maxx :: PRFunction
maxx = comp add [p 1, comp sub [p 2,p 1]]

-- A3
orr :: PRFunction
orr = maxx

neq :: PRFunction
neq = comp nott [eq]

-- A4
--f = comp add [p 1, comp half [comp mul comp add] ]
--f = add•(π13 x half•(mul•(add•(π31 x π33)) x (add•(π33 x add2•π23))))


exp2m1 :: PRFunction
exp2m1 = comp sub [comp expp [p 1,const 2], const 1]

dist :: PRFunction
dist = comp add [comp sub [p 1,p 2], comp sub [p 2,p 1]]

numseq :: PRFunction
numseq = pr numseq (const 1) (comp add [p 1,p 2])


main = do
  print( expp [2,4] )
  print( maxx [5,8] )
  print( maxx [10,5] )
  print( orr [1,0] )
  print( orr [1,1] )
  print( orr [1,1] )
  print( orr [0,0] )
  print( neq [1,10] )
  print( neq [0,0])
  print( dist [4,8] )
  print( dist [10,8] )
  print( map numseq [[x]|x<-[0..10]] )
