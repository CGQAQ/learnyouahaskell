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
tailFib cur next arr n = tailFib next (cur+next) (arr++[cur]) $ pred n}

-- High order function in Haskell >\=

0