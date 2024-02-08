module Lib
    ( decodeMessage
    ,decodeMessageImproved
    ) where


-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Nothing
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Nothing
--
-- >>> decodeMessage "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- Just 3
-- 
-- >>> decodeMessage "5 5 5 1 2 3 4 8 2 3"
-- Nothing
decodeMessage :: String -> Maybe Int
decodeMessage msg =
        let numbers = map read (words msg) :: [Int]
            -- Unique, min and max numbers
            uniqueNumbers = filterUnique numbers
            minNumber = minimum uniqueNumbers
            maxNumber = maximum uniqueNumbers
            --Checks 
            evenSum = checkEvenSum minNumber maxNumber
            checkMinUniqueResult = checkUnique numbers minNumber
            checkMaxUniqueResult = checkUnique numbers maxNumber
            --Magic number and occurence of magic number
            magicNumber = (minNumber + maxNumber) `div` 2
            magicNumberCount = ocurrencesOfMagicNumber numbers magicNumber
        in
            if evenSum && checkMinUniqueResult && checkMaxUniqueResult
                then Just magicNumberCount
                else Nothing


-- >>> filterUnique [1,2,3,4,5]
-- [1,2,3,4,5]
-- >>> filterUnique [1,1,3,4,5]
-- [1,3,4,5]
-- >>> filterUnique [1,2,3,4,5,3,3,3,3,3]
-- [1,2,3,4,5]
filterUnique :: [Int] -> [Int]
filterUnique [] = []
filterUnique [x] = [x]
filterUnique(x:y:xs)
-- If the first element x is equal to the second element y, remove x and call function again w/ the remaining list
    | x == y = filterUnique (y:xs)  
-- If they are not equal, keep x and call function again w/ the remaining list
    | otherwise = x : filterUnique (y:xs)

-- >>> ocurrencesOfMagicNumber [1,2,3,4,5] 3
-- 1
-- >>> ocurrencesOfMagicNumber [1,2,3,4,5] 6
-- 0
-- >>> ocurrencesOfMagicNumber [1,2,3,4,5,3,3,3,3,3] 3
-- 5
ocurrencesOfMagicNumber :: [Int] -> Int -> Int
ocurrencesOfMagicNumber [] _ = 0
ocurrencesOfMagicNumber (x:xs) magicNumber
    | x == magicNumber = 1 + ocurrencesOfMagicNumber xs magicNumber
    | otherwise = ocurrencesOfMagicNumber xs magicNumber

-- >>> checkEvenSum 7 9
-- True
-- >>> checkEvenSum 2 4
-- True
-- >>> checkEvenSum 3 4
-- False
-- >>> checkEvenSum 1 9
-- True
checkEvenSum :: Int -> Int -> Bool
checkEvenSum x y = (x + y) `mod` 2 == 0

-- >>> checkUnique [1,2,3,4,5] 1
-- True
-- >>> checkUnique [1,1,3,4,5] 1
-- False
-- >>> checkUnique [1,2,3,4,5,3,3,3,3,3] 3
-- False
checkUnique :: [Int] -> Int -> Bool
-- Filters out ther numbers equal to x
-- Checks if the new list has a length of 1 (if so it is unique)
checkUnique numbers x = length (filter (==x)numbers) == 1

-- | Decode an intergalactic message from a string.
-- The message is a sequence of integers separated by spaces.
-- This is an improved version of the previous function, with a more
-- informative error messages.
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 1"
-- Left "Communication interference detected: minimum number not unique"
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 9"
-- Left "Communication interference detected: maximum number not unique"
--
-- >>> decodeMessageImproved "5 5 5 8 1 2 3 4 9 8 2 3 4 8"
-- Right 3
--
-- >>> decodeMessageImproved "5 5 5 1 2 3 4 8 2 3"
-- Left "Communication interference detected: midPoint not even"
decodeMessageImproved :: String -> Either String Int
decodeMessageImproved msg =
        let numbers = map read (words msg) :: [Int]
            -- Unique, min and max numbers
            uniqueNumbers = filterUnique numbers
            minNumber = minimum uniqueNumbers
            maxNumber = maximum uniqueNumbers
            --Checks 
            evenSum = checkEvenSum minNumber maxNumber
            checkMinUniqueResult = checkUnique numbers minNumber
            checkMaxUniqueResult = checkUnique numbers maxNumber
            --Magic number and occurence of magic number
            magicNumber = (minNumber + maxNumber) `div` 2
            magicNumberCount = ocurrencesOfMagicNumber numbers magicNumber
        in
            if evenSum && checkMinUniqueResult && checkMaxUniqueResult
                then Right magicNumberCount
                else Left ("Communication interference detected: " ++ 
                    (if not evenSum then "midPoint not even" else "")++
                    (if not checkMinUniqueResult then "minimum number not unique" else "")++
                    (if not checkMaxUniqueResult then "maximum number not unique" else "")
                    )