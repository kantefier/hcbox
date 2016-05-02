module SortingAlgs
( bubbleSort ) where

findMax :: (Ord a) => [a] -> a
findMax (x:[]) = x
findMax (x:y:[]) = max x y
findMax (x:xs) = max x (findMax xs)

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted (x:[]) = True
sorted lst @ (x:y:xs) = x <= y && sorted(tail lst)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort (x:[]) = x:[]
bubbleSort lst = until(sorted)(\(x:y:xs) -> (min x y) : bubbleSort((max x y) : xs)) lst