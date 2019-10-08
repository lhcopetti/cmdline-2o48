module Arrays
    ( replace
    ) where


replace :: Int -> a -> [a] -> [a]
replace i v xs 
    | i < 0 = error "Negative indexes are invalid"
    | i >= length xs = error $ "The index " ++ show i ++ " is invalid for the list of length " ++ show (length xs)
    | otherwise = take i xs ++ [v] ++ drop (i+1) xs
