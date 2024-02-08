module Lab1 where
-- | mhead is my own implementation of head function.
--
-- >>> mhead [1,2,3]
-- 1
--
-- >>> mhead ['a','b','c']
-- 'a'
--
-- >>> mhead "Hello"
-- 'H'

    --Task 3: AddNumber uses a as a type and prefaces that ot should be a number
    -- by stating that for any type A that is a member of the Num class take two parameters a
    -- and return a value of type a (asked chatGPT for explanation of this)
addNumber:: Num a => a -> a -> a
addNumber a b = a + b

    --Task 3: Version A, stating that Age is derived from Int, passing a generic number leads
    --to a compiler error
newtype Age = Age Int deriving (Show)

addAge :: Age -> Age -> Age
addAge (Age a) (Age b) = Age (a + b)

class Addable a where
    addAgeB :: a -> a -> a
    -- addAgeB (Age x) (Age y)
instance Addable Age where
    addAgeB (Age a) (Age b) = Age (a + b)
    -- addAgeB (x::Int) y
instance Addable Int where
    addAgeB a b = a + b 
    
    
    --Task 4: mhead returns the first element of a list
mhead :: [a] -> a
mhead [] = error "Empty list"
mhead (x:xs) = x 

    --Task 4: more implementations of mhead
    -- with help from chatgpt
mheadPattern :: [a] -> a
mheadPattern [] = error "Empty list"
mheadPattern (x:_) = x

mheadCase :: [a] -> a
mheadCase xs = case xs of
    [] -> error "Empty list"
    (x:_) -> x
    
mheadLambda :: [a] -> a
mheadLambda = \xs -> case xs of
    [] -> error "Empty list"
    (x:_) -> x
    
mheaMaybe :: [a] -> Maybe a
mheaMaybe [] = Nothing
mheaMaybe (x:_) = Just x

        -- Task 1:Write Hello world
main :: IO()
main = do
        putStrLn "Hello world"
        --Task 2:Hello name
        putStrLn "What is your name?"
        name <- getLine
        putStrLn("Hello " ++ name)
        --Task 3: What is your age
        putStrLn "What is your age?"
        age <- getLine
        putStrLn("Your age is " ++ age)
        putStrLn("Hi " ++ name ++ " in 10 years you will be " ++ show(read age+10))