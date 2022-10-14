module Exercise.Ex01 (myLast)  where


myLast :: Ord a => [a] -> a
myLast lst = 
  case lst of
    [] -> error "Exception: calling myLast on empty list"
    [x] -> x
    (_:xs) -> myLast xs