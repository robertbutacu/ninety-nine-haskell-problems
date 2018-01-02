{--
(**) Determine whether a given integer number is prime.
--}

import Data.List
import Control.Monad

isPrime ::  Int -> Bool
isPrime n | n < 4 = n /= 1
isPrime n = all (== False) (map (\x -> mod n x == 0) [2..(floor $ sqrt $ fromIntegral n)])


{--
(**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
--}
myGcd :: (Ord a, Num a, Integral a) => a -> a -> a
myGcd x y 
     | (x - y) == 0 = x
     | (x - y) <  0 = gcd x        ( y - x )
     | otherwise    = gcd ( x - y) y

{--
(*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
--}
isCoprime :: (Ord a, Num a, Integral a) => a -> a -> Bool
isCoprime x y = if myGcd x y == 1 then True else False


{--
(**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
--}

phi :: Integer -> [Integer]
phi m = coprimes [1..(fromIntegral (m - 1))]
      where coprimes = filter (\x -> isCoprime m x)

{--
(**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
--}

primeFactors :: Int -> [Int]
primeFactors n = filter (\x -> mod n x == 0) $ primes
         where primes = filter isPrime [1..(n-1)]
               

{--

(**) Determine the prime factors of a given positive integer.

Construct a list containing the prime factors and their multiplicity.
--}
primeFactorsMultiplicity n = map transform ( primeFactors n )
                           where transform x = (x, withMultiplicity x)
                                 withMultiplicity x = go n x 0
                                 go m p q = if mod m p == 0 then go (div m p) p (q + 1) else q 


{--
(**) Calculate Euler's totient function phi(m) (improved).

See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:

phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         (p2 - 1) * p2 ** (m2 - 1) * 
         (p3 - 1) * p3 ** (m3 - 1) * ...
Note that a ** b stands for the b'th power of a.
--}


{--
(*) Compare the two methods of calculating Euler's totient function.

Use the solutions of problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example.

(no solution required)
--}




{--
(*) A list of prime numbers.

Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

Example in Haskell:
--}

primesInRange :: Int -> Int -> [Int]
primesInRange lower upper = takeWhile (<= upper) . dropWhile (<= lower) . filter isPrime $ [1..]

primesInRange2 :: Int -> Int -> [Int]
primesInRange2 lower upper = filter isPrime $ [lower..upper]

primesInRange3 :: Int -> Int -> [Int]
primesInRange3 lower upper = [x | x <- [lower..upper] , isPrime x]


{--
11 Problem 40
(**) Goldbach's conjecture.

Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
--}

allGoldbachs :: Int -> [(Int, Int)]
allGoldbachs n = map (\x -> (x, n - x))  ( filter hasMatch primes )
           where primes = filter isPrime [1..n]
                 hasMatch x = elem (n - x) primes


firstGoldbach :: Int -> Maybe (Int, Int)
firstGoldbach n = if even n then computeGoldbach n else Nothing

computeGoldbach :: Int -> Maybe (Int, Int)
computeGoldbach n = hasFound  ( find firstMatch primes )
                    where primes = filter isPrime [1..n]
                          firstMatch x = elem (n - x) primes
                          hasFound a = case a of Just y  -> Just (y, subtract y n)
                                                 Nothing -> Nothing
{--
(**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
--}

goldbachsInRange lower upper = map (\x -> (x, allGoldbachs x)) [x | x <- [lower..upper], even x]

goldbachsInRange2 lower upper primeLowerLimit = filter (\x -> isBigDifference (snd x)) $ goldbachs
                                where isBigDifference a = all isValid a
                                      goldbachs = goldbachsInRange lower upper
                                      isValid p = ((fst p ) > primeLowerLimit) && ((snd p) > primeLowerLimit)