{-#  LANGUAGE NPlusKPatterns  #-}

{--
    Primitive recursive functions in Haskell
    Tuples of natural numbers are represented by lists

    Original version: WS-99-00 Prof. Dr. Raul Rojas
    modified WS-12-13 by: Prof. Dr. Margarita Esponda
--}

---------------------- Basic functions ---------------------------------

module PRF where

type PRFunction = ([Integer]->Integer)

-- Zero function

z :: PRFunction
z xs = 0

--Succesor function

s :: PRFunction
s [x] = x+1

--Projection functions

p::Integer -> [Integer] -> Integer
p 1 (a:b) = a
p n (a:b) = p (n-1) b

---------------------- Metha-functions --------------------------------

-- Composition

comp :: PRFunction -> [PRFunction] -> [Integer] -> Integer
comp h gs xs = h [g xs | g<-gs ]

-- Primitive recursion

pr :: PRFunction -> PRFunction -> PRFunction -> [Integer] -> Integer

pr rec g h (  0  :xs) = g xs
pr rec g h ((n+1):xs) = h ( (rec (n:xs)):n:xs )

-----------------------------------------------------------------------
-- Help function (Convert an argument to a list)

arg2list x = [x]

----------- some examples of primitive recursive functions ------------

-- Predecessor

predd :: PRFunction
predd  = pr predd (const 0) (p 2)

-- Addition

add :: PRFunction
add  = pr add (p 1) (comp s [(p 1)])

-- Test of equality to zero

isZero :: PRFunction
isZero  = pr isZero (const 1) (const 0)

-- Test of positive

isPositive :: PRFunction
isPositive = pr isPositive (const 0) (const 1)

-- smaller or equal (<=)

leq :: PRFunction
leq = comp isZero [sub]

-- smaller (<) and greater (>)

smaller :: PRFunction
smaller = comp isPositive [comp sub [(p 2), (p 1)]]

greater = comp isPositive [comp sub [(p 1), (p 2)]]

-- greater or equal (>=)

geq :: PRFunction
geq = comp isZero [comp sub [(p 2), (p 1)]]

-- Multiplication

mul :: PRFunction
mul  = pr mul z (comp add [(p 1),(p 3)])

-- Subtraction

sub :: PRFunction
sub  = comp sub' [(p 2),(p 1)]
       where
         sub'  = pr sub' (p 1) (comp predd [(p 1)])

-- Logical operators

nott :: PRFunction
nott  = isZero

andd :: PRFunction
andd  = mul

-- equal

eq :: PRFunction
eq  = comp andd [comp geq [(p 1), (p 2)], comp leq [(p 1), (p 2)]]

-- True if n is odd

odd2 [n] = pr odd2 (const 0) (comp nott [(p 1)]) [n]

-- True if n is even

even2 [n] = nott[odd2 [n]]

-- divide with 2

half [n] = pr half (const 0) (comp add [(p 1),(comp odd2 [(p 2)])]) [n]

-- Factorial

fact [n] = pr fact (const 1) (comp mul [(p 1),(comp s [(p 2)])]) [n]

-- The ndiv function is not defined for y=0

ndiv [x,y] = (comp ndiv' [(p 1), (p 2), (p 1)]) [x,y] -- first call with ndiv' [x, y, x]

ndiv' [n,y,x] = pr ndiv' g h [n,y,x]
               where
               g = const 0
               h = comp add [(p 1), comp isZero [ comp sub [ comp mul [comp s [(p 2)], (p 3)], (p 4)]]]

-- constant function

-- const            :: a -> b -> a
-- const x _        =  x

