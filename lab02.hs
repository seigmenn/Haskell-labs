module Lab02 where
import Text.Printf (printf)
    -- | mreverse is my own implementation of list reversal
--
-- >>> mreverse "Hello"
-- "olleH"
--
-- >>> mreverse [1,2,3]
-- [3,2,1]

-- Task 1: Reverse a list
mreverse :: [a] -> [a]
mreverse [] = []
mreverse (x:xs) = mreverse xs ++ [x]

-- Task 2: multiplication table for n x n numbers, with padding to format nicely
multiplicationTable :: Int -> String
multiplicationTable n = unlines [unwords [formatCell (x * y) | x <- [1..n]] | y <- [1..n]]
  where
    cellWidth = length (show (n * n)) + 1
    formatCell = printf ("%-" ++ show cellWidth ++ "d")

-- Task 3: read data from txt file
data Student = Student {firstName :: String, lastName:: String, age :: String}

parseStudent :: String -> Student
parseStudent line = Student {firstName = firstName, lastName = lastName, age = age}
  where
    [firstName, lastName, age] = words line

oldestStudentCount :: [Student] -> Int
oldestStudentCount students = 
    length [ student | student <- students, read (age student) == oldestAge]
    where
        oldestAge = maximum [read (age student)::Int | student <- students]


main:: IO()
main = do
    input <- getContents
    let students = map parseStudent (lines input)
        count = oldestStudentCount students
    putStrLn ("There are " ++ show count ++ " oldest students")
