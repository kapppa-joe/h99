module Exercise.H99.LogicAndCodes where

import Control.Monad (replicateM)

table :: (Bool -> Bool -> Bool) -> [[Bool]]
table f = 
    [[a, b, f a b] | a <- [True, False], b <- [True, False]]

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

not' :: Bool -> Bool
not' True = False
not' False = True

nand :: Bool -> Bool -> Bool
nand a b = not' $ and' a b

nor :: Bool -> Bool -> Bool
nor a b = not' $ or' a b

xor :: Bool -> Bool -> Bool
xor True = not'
xor False = id

equ :: Bool -> Bool -> Bool
equ a b = not' $ xor a b

infixl 4 `or'`
-- infixl 4 `nor`
-- infixl 5 `xor`
infixl 6 `and'`
-- infixl 6 `nand`
infixl 3 `equ`

printTruthTable :: [[Bool]] -> IO ()
printTruthTable tbl = mapM_ putStrLn $ [makeRow bools | bools <- tbl]
  where
    makeRow :: [Bool] -> String
    makeRow [] = ""
    makeRow [x, y] = show x ++ " => " ++ show y
    makeRow (x:xs) = show x ++ " " ++ makeRow xs

tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = [args ++ [f args] | args <- replicateM n [True, False]]