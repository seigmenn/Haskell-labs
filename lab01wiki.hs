
module Lab1 where
    import Data.Char (toUpper)

    -- Write a function that given a name returns the name with the first character capitalized
    capitalize :: String -> String
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs

    -- Write a function that given an arbitrary number of numbers in a single line returns their product
    productOfNumber :: [Double] -> Double
    productOfNumber [] = 1
    productOfNumber (x:xs) = x * productOfNumber xs

    -- Write Hello world
    main :: IO()
    main = do
        putStrLn "Hello world"

    -- Read two numbers from standard input and return their sum
        putStrLn "Enter two numbers:"
        a <- readLn
        b <- readLn
        print (a + b)

    -- Read three names from standard input given in a single line and return the second one
        putStrLn "Enter three names:"
        names <- getLine
        let nameList = words names
        print (nameList !! 1)

    -- Read a name from standard input and return the second character of that name
        putStrLn "Enter a name:"
        name <- getLine
        print (name !! 1)