module Exercise.H99.LogicAndCodes where

table :: (Bool -> Bool -> Bool) -> [[Bool]]
table f = 
    [[a, b, f a b] | a <- [True, False], b <- [True, False]]