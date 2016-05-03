module SortingAlgs
( bubbleSort ) where

import Data.List (partition)

findMax :: (Ord a) => [a] -> a
findMax (x:[]) = x
findMax (x:y:[]) = max x y
findMax (x:xs) = max x (findMax xs)

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted (_:[]) = True
sorted lst @ (x:y:xs) = x <= y && sorted(tail lst)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort a @ (_:[]) = a
bubbleSort lst = until(sorted)(\(x:y:xs) -> (min x y) : bubbleSort((max x y) : xs)) lst

selectSort :: (Ord a) => [a] -> [a]
selectSort [] = []
selectSort a @ (_:[]) = a
selectSort lst =
	minElem : (selectSort restList)
	where
		indexedLst = zip lst [0,1..]
		(minElem, minElemIdx) = minimum indexedLst
		(restList, _) = unzip(filter(\(x, idx) -> idx /= minElemIdx) indexedLst)