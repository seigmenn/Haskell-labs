module Lab02 where
    -- write your own mreverse function without using the built-in reverse
    mreverse :: [a] -> [a]
    mreverse [] = []
    mreverse (x:xs) = mreverse xs ++ [x]

    -- write your own mlength function without using the built-in length
    mlength :: [a] -> Int
    mlength [] = 0
    mlength (x:xs) = 1 + mlength xs

    -- write a function that generates the first n numbers of the fibonnaci numbers
    fibonnaci :: Int -> [Int]
    fibonnaci 0 = []
    fibonnaci 1 = [1]
    fibonnaci 2 = [1, 1]
    fibonnaci n = fibonnaci (n - 1) ++ [last (fibonnaci (n - 1)) + last (fibonnaci (n - 2))]

    -- be able to use simple functions with map and foldl
    -- function that squares each element of a list using map
    square :: [Int] -> [Int]
    square [] = []
    square numbers = map (\x -> x*x) numbers

    -- function that sums all elements of a list using foldl
    sumList :: [Int] -> Int
    sumList = foldl (\x y -> x + y) 0