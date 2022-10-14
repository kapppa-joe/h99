module Exercise.Ex01 (myLast, myButLast)  where


myLast :: Ord a => [a] -> a
myLast lst = 
  case lst of
    [] -> error "Exception: empty list"
    [x] -> x
    (_:xs) -> myLast xs

myButLast :: Ord a => [a] -> a
myButLast _ = error "not implemented"
