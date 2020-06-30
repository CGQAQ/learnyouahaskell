{-
   _____ ______                      
   \    \\     \                     
    \    \\     \                    
     \    \\     \ _____________     
      \    \\     \\            \    
       \    \\     \\____________\   
        \    \\     \ _____________  
        /    //      \\            \ 
       /    //        \\____________\
      /    //    /\    \             
     /    //    /  \    \            
    /    //    /    \    \           
   /____//____/      \____\          
    _    _           _        _ _   
   | |  | |         | |      | | |  
   | |__| | __ _ ___| | _____| | |  
   |  __  |/ _` / __| |/ / _ \ | |  
   | |  | | (_| \__ \   <  __/ | |  
   |_|  |_|\__,_|___/_|\_\___|_|_|  

-}


module Main where

main :: IO()
main = do
    print("hello world")


-- fib :: Integer -> Integer -> Integer
-- fib 0 1 = 0
-- fib 1 1 = 1
-- fib n a = fib (n-2) n-1

-- fibn :: Integer -> Integer
-- fibn n = fib n n+1

-- fibo a b = a:fibo b (a+b)

-- fibon n = take n $ fibo 0 1

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  
 
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum' of empty list"
maximum' [x] = x
maximum' (x:xs)
    |x > maxTail = x
    |otherwise = maxTail
    where maxTail = maximum' xs


-- Sort
sort' :: (Ord a) => [a] -> [a]
sort' [] = []
-- sort' [x] = [x]
sort' (x:xs) =
    let smallerSort = sort' [a | a <- xs, a <= x]
        biggerSort = sort' [a|a <- xs, a>x]
    in smallerSort ++ [x] ++ biggerSort

-- Tail recursion Fibonacci
tailFib :: (Num a, Eq a, Enum a) => a -> a -> [a] -> a -> [a]
tailFib _  _  result 0 = result
tailFib cur next arr n = tailFib next (cur+next) (arr++[cur]) $ pred n

-- High order function in Haskell 
sort'' :: (Ord a) => [a] -> [a]
sort'' [] = []
sort'' (x:xs) =
    let smallerSort = sort'' $ filter (<x) xs
        biggerSort = sort'' $ filter (>=x) xs
    in smallerSort ++ [x] ++ biggerSort

-- Largest number less than 100,000 can divided by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head $ filter p [100000, 99999..]
    where p x = x `mod` 3829 == 0

-- find the sum of all odd squares that are smaller than 10,000.
oddSquaresLessThan10'000 :: (Integral a) => a
oddSquaresLessThan10'000 = sum $ takeWhile (<10000) $ filter odd $ map (^2) $ [1..]

oddSquaresLessThan10'000' :: (Integral a) => a
oddSquaresLessThan10'000' = sum $ takeWhile (<10000) $ [oddSquare|x <- [1..], let oddSquare = x^2, (odd oddSquare)]

{-
For our next problem, we'll be dealing with Collatz sequences. 
We take a natural number. If that number is even, we divide it by two. 
If it's odd, we multiply it by 3 and then add 1 to that. 
We take the resulting number and apply the same thing to it, 
which produces a new number and so on. In essence, we get a chain of numbers.
 It is thought that for all starting numbers, the chains finish at the number 1. 
 So if we take the starting number 13, we get this sequence: 13, 40, 20, 10, 5, 16, 8, 4, 2, 1. 13*3 + 1 equals 40. 
     40 divided by 2 is 20, etc. We see that the chain has 10 terms.
-}

-- for all starting numbers between 1 and 100, how many chains have a length greater than 15? 
collatzSeq :: (Integral a, Num a) => a -> [a]
collatzSeq 1        = [1]
collatzSeq x
    | (odd x)    = x:(collatzSeq $ x*3+1)
    | (even x)   = x:(collatzSeq $ x `div` 2)
    | otherwise = []
--chainCount :: Int
chainCount = length $ filter (>15) $ map length $ map collatzSeq [1..100]