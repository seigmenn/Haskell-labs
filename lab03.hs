module Lab3 where

-- Task 1 - implementation of mhead
--In this variant, use function argument pattern matching.
mhead1 :: [a] -> a
mhead1 [] = error "Empty list"
mhead1 (x:_) = x

--In this variant, use function guards
mhead2 :: [a] -> a
mhead2 xs
    | not (null xs) = let (x:_) = xs in x
    | otherwise = error "Empty list"

--In this version, write the definition in a single line with if ... else ... expressions
mhead3 :: [a] -> a
mhead3 xs = if null xs then error "Empty list" else let (x:_) = xs in x

--In this variant, make use of let .. in ..
mhead4 :: [a] -> a
mhead4 xs = let (x:_) = xs in x

--In this variant, use where expression
mhead5 :: [a] -> a:
mhead5 xs = x
    where (x:_) = xs

--In this variant, use case .. of .. expression
mhead6 :: [a] -> a
mhead6 xs = case xs of
    [] -> error "Empty list"
    (x:_) -> x

-- Task 2 - factorial
mfact :: Integer -> Integer
mfact n
    | n < 0 = error "Negative number"
    | n == 0 = 1
    | otherwise = n * mfact (n-1)


-- Task 3 - Fibonacci - return the nth number of the sequence
fib :: Integer -> Integer
fib n
    | n < 0 = error "Negative number"
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib(n) + fib(n-1)

-- Task 4 - Fibonacci
-- Q: What is (b:_) for?
-- A: Where a:t determines the head and tail of the list sent, (b:_) takes the tail t of the list  and further breaks it down into the head b and a tail.
-- Q: What does the t@ do?
-- A: @ is used to give a name to a sub-pattern, which in this case is the (b:_). The t@ is used to give this pattern a name. this means one can refer to the entire tail using t
-- and you won't have to repeat the pattern (b:_) again.
fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t

fib2 :: Int -> Integer --There is a difference between Int and Integer in Haskell. Int is bounded (has a min, max value based on the machine) while Integer is unbounded (more arbitrary).
fib2 n = fibs !! n

-- Task 5 - Fibonacci zip, zipWith
-- zip: takes two lists and combines the elements into pairs
-- zipWith: takes a function and two lists and combines the elements into pairs using the function
-- 0 and 1 are the initial values of the fibonacci sequence, zipWith adds the list fibs with its own tail and results in a new list with the sum of the two previous elements
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- The actual question is: using sum, repeat, and zip, write a function, that counts how many times a given number appears in a given list. For example, how many times, 
--number 10, appears in a given list. count 10 [2,10,3,10,4] should return 2
count :: Eq a => a -> [a] -> Int
count x = sum . map(\(y,z) -> if y == z then 1 else 0) .zip(repeat x)
-- Creates an infinite list only consisting of the number we're searching for, we zip this with the other list which results in a list of tuples all being [some number, x]
-- then using map and a lambda function we check whether the two numbers in the tuple are equal and return either 1 or 0
-- sum counts all the ones and zeros and returns the result