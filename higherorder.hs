import Data.List
multThree :: Num a => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: Integer -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (t -> t) -> t -> t
applyTwice f x = f(f x)

mymap :: (t -> a) -> [t] -> [a]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs)
    | f x       = x : filter f xs
    | otherwise = filter f xs

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub -- Equivale a numUniques xs = length(nub xs)