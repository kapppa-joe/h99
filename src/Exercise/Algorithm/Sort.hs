module Exercise.Algorithm.Sort where

mergeSort :: Ord a => [a] -> [a]
mergeSort xs = mergeAll $ map (\x -> [x]) xs


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xt) ys@(y:yt)
  | x <= y  = x : merge xt ys 
  | otherwise = y : merge xs yt

mergeAll :: Ord a => [[a]] -> [a]
mergeAll (x:y:zs) = merge (merge x y) (mergeAll zs)
mergeAll [list] = list
mergeAll [] = []


quickSort ::  Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = smaller ++ [x] ++ larger
  where 
    smaller = quickSort $ filter (< x) xs
    larger = quickSort $ filter (>= x) xs

quickSortBy :: Ord b => [a] -> (a -> b) -> [a]
quickSortBy [] _ = []
quickSortBy [x] _ = [x]
quickSortBy (x:xs) f = smaller ++ [x] ++ larger
  where
    fx = f x
    smaller = quickSortBy (filter (\y -> f y < fx) xs) f
    larger = quickSortBy (filter (\y -> f y >= fx) xs) f