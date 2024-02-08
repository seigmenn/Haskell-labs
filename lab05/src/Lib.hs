module Lib
    ( countScore
    ) where

-- | Count the total score of the four dice game data.
-- This function is already implemented for you.
-- 
countScore :: String -> Int
countScore txt = sum $ map processLine (lines txt)


-- | Process a single line of the input data.
--
-- >>> processLine "4 21 10 5 4 21 13 11"
-- 5
-- >>> processLine "4 21 10 5 4 21 13 11 10"
-- 7
--
-- >>> processLine "4 21 10 5 4 21 13 11 10 10"
-- 11
--
-- >>> processLine "10 21 10 5 4 21 13 11 10"
-- 8
--
-- >>> processLine "10 21 10 5 8 20 13 11"
-- 0
-- >>> processLine "10 10 10 5 4 21 13 11 10 10 10"
-- 56
--
-- >>> processLine "8 14 16 5 8 14 16 14"
-- 9
-- 
-- >>> processLine "8 14 16 5 8 18 16 12"
-- 3
processLine :: String -> Int
processLine line =
    let (winningNumbers, rolls) = splitAt 3 $ map read $ words line
        initialRolls = take 3 rolls
        restRolls = drop 3 rolls
        initialWinningCounts = countOccurrences initialRolls winningNumbers
        winningCounts = countOccurrences rolls winningNumbers
        initialPoints = sum $ map (calculatePoints winningNumbers) initialRolls
        totalPoints = initialPoints + calculateBonusPoints initialWinningCounts
        restPoints = sum $ map (calculatePoints winningNumbers) restRolls
    in totalPoints + restPoints

countOccurrences :: Eq a => [a] -> [a] -> Int
countOccurrences list targets = sum $ map (\target -> length $ filter (== target) list) targets


calculatePoints :: [Int] -> Int -> Int
calculatePoints winningNumbers roll
    | roll `elem` winningNumbers = calculateSinglePoint roll winningNumbers
    | otherwise = 0

calculateSinglePoint :: Int -> [Int] -> Int
calculateSinglePoint roll winningNumbers
    | roll `elem` [4..9] = 1
    | roll `elem` [10..19] = 2
    | roll `elem` [20..24] = 4
    | otherwise = calculateRepeatPoints [roll] winningNumbers


calculateBonusPoints :: [Int] -> Int
calculateBonusPoints counts = sum [bonusPoints count | count <- counts]

calculateRepeatPoints :: [Int] -> [Int] -> Int
calculateRepeatPoints initialRolls winningNumbers =
    let repeatedWinningNumbers = filter (\x -> countOccurrences initialRolls [x] == 2) winningNumbers
        counts = concatMap (\x -> countOccurrences initialRolls [x]) repeatedWinningNumbers
    in sum $ map bonusPoints counts

bonusPoints :: Int -> Int
bonusPoints count
    | count == 2 = 2  -- Double points for a winning number repeated once
    | count >= 3 = count * 4  -- Quadruple points for a winning number repeated thrice or more
    | otherwise = 0  -- No bonus points for other cases



