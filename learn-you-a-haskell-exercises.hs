doubleMe x = x * 2

isPrime x = (filter (\y -> x `mod` y == 0) [2..(x - 1)]) == []

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

apply' :: (a -> a) -> a -> a
apply' f x = f x

oddIndexes :: (Num a) => [a] -> [a]
oddIndexes [] = []
oddIndexes (x:[]) = [x]
oddIndexes all@(x:y:xs) = [x] ++ (oddIndexes xs)

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  


oddIndexes2 :: (Num a) => [a] -> [a]
oddIndexes2 xs = case xs of [] -> []
                            x:[] -> [x]
                            x:y:t -> [x] ++ oddIndexes2 t


fibonacci :: (Num a, Ord a, Num p, Eq a) => a -> p
fibonacci n
        | n <= 2     = 1 
        | otherwise = fibonacci (n - 1) + fibonacci (n - 2)                                         


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    (quicksort (filter (\y -> y < x) xs)) ++ [x] ++ quicksort (filter (\y -> y > x) xs)

quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 (x:xs) = 
    let smaller = quicksort [y | y <- xs, y <= x]
        bigger  = quicksort [y | y <- xs, y > x]
    in smaller ++ [x] ++ bigger

biggestDivisor :: (Integral a, Num a) => a -> a
biggestDivisor n = head (filter isDivisor [2..n])
    where isDivisor x = n `mod` x == 0

biggestDivisor2 :: (Ord a, Eq a, Num a) => a -> a -> a
biggestDivisor2 x y = bigD x y
   where bigD x y = if x == y then x else biggestDivisor2 ((max x y) - (min x y)) (min x y)


--find the sum of all odd squares that are smaller than 10,000
getSum = sum . takeWhile (< 10000) $ [ x ^ 2 | x <- [1..], odd (x ^ 2) ]

getSum2 = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1..]


getSum3 = 
    let odds = filter odd [x ^ 2 | x <- [1..] , odd (x ^ 2)]
        belowLimit = takeWhile (<10000) odds
    in sum belowLimit

{-
For our next problem, we'll be dealing with Collatz sequences. 
We take a natural number. 
If that number is even, we divide it by two. 
If it's odd, we multiply it by 3 and then add 1 to that. 
We take the resulting number and apply the same thing to it, which produces a new number and so on. 
In essence, we get a chain of numbers. It is thought that for all starting numbers, the chains finish at the number 1. 
So if we take the starting number 13, we get this sequence: 13, 40, 20, 10, 5, 16, 8, 4, 2, 1. 13*3 + 1 equals 40. 
40 divided by 2 is 20, etc.
 We see that the chain has 10 terms.

Now what we want to know is this: for all starting numbers between 1 and 100, how many chains have a length greater than 15? 
First off, we'll write a function that produces a chain:
-}

chain 1 = [1]
chain n 
    | odd n  = n : chain (n * 3 + 1)
    | even n = n : chain (n `div` 2)

chainsWithLength = length (filter isLengthy (map chain [1..100]))
    where isLengthy x = length x > 15
    
    
add5 n = n + 5
multiplyBy10 n = n * 10

 