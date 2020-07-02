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
        biggerSort = sort' [a | a <- xs, a>x]
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
oddSquaresLessThan10'000' = sum $ takeWhile (<10000) $ [oddSquare | x <- [1..], let oddSquare = x^2, (odd oddSquare)]

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



-- maximum fold
maximum'fold :: (Integral a, Ord a) => [a] -> a
maximum'fold x = foldr1 max x


-- reverse' fold
reverse'fold :: (Integral a) => [a] -> [a]
reverse'fold x = foldl (\acc x -> x:acc) [] x


-- product'fold
product'fold :: (Integral a) => [a] -> a
product'fold x = foldl1 (*) x
-- filter'fold
filter'fold :: (Integral a) => (a->Bool) -> [a] -> [a]
filter'fold p = foldr (\x acc -> if p x then x:acc else acc) []
-- head'fold
head'fold :: (Integral a) => [a] -> a
head'fold = foldr1 (\x _ -> x)
-- last'fold
last'fold :: (Integral a) => [a] -> a
last'fold = foldr1 (\_ x -> x)


{-

ghci> intersperse '.' "MONKEY"  
"M.O.N.K.E.Y"  
ghci> intersperse 0 [1,2,3,4,5,6]  
[1,0,2,0,3,0,4,0,5,0,6]  

ghci> intercalate " " ["hey","there","guys"]  
"hey there guys"  
ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]  
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]  

ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]  
[[1,4,7],[2,5,8],[3,6,9]]  
ghci> transpose ["hey","there","guys"]  
["htg","ehu","yey","rs","e"]  

ghci> concat ["foo","bar","car"]  
"foobarcar"  
ghci> concat [[3,4,5],[2,3,4],[2,1,1]]  
[3,4,5,2,3,4,2,1,1]  

ghci> concatMap (replicate 4) [1..3]  
[1,1,1,1,2,2,2,2,3,3,3,3]  

ghci> and $ map (>4) [5,6,7,8]  
True  
ghci> and $ map (==4) [4,4,4,3,4]  
False  

ghci> or $ map (==4) [2,3,4,5,6,1]  
True  
ghci> or $ map (>4) [1,2,3]  
False  

ghci> any (==4) [2,3,5,6,1,4]  
True  
ghci> all (>4) [6,9,10]  
True  
ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
False  
ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
True  

ghci> take 10 $ iterate (*2) 1  
[1,2,4,8,16,32,64,128,256,512]  
ghci> take 3 $ iterate (++ "haha") "haha"  
["haha","hahahaha","hahahahahaha"]  

ghci> splitAt 3 "heyman"  
("hey","man")  
ghci> splitAt 100 "heyman"  
("heyman","")  
ghci> splitAt (-3) "heyman"  
("","heyman")  
ghci> let (a,b) = splitAt 3 "foobar" in b ++ a  
"barfoo"  

ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]  
[6,5,4]  
ghci> takeWhile (/=' ') "This is a sentence"  
"This" 

ghci> sum $ takeWhile (<10000) $ map (^3) [1..]  
53361  

ghci> dropWhile (/=' ') "This is a sentence"  
" is a sentence"  
ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]  
[3,4,5,4,3,2,1]  

We're given a list that represents the value of a stock by date. The list is made of tuples whose first component is the stock value, the second is the year, the third is the month and the fourth is the date. We want to know when the stock value first exceeded one thousand dollars!
ghci> let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
ghci> head (dropWhile (\(val,y,m,d) -> val < 1000) stock)  
(1001.4,2008,9,4)  

ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest  
"First word: This, the rest: is a sentence"  

ghci> break (==4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])  
ghci> span (/=4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])  

ghci> sort [8,5,3,2,1,6,4,2]  
[1,2,2,3,4,5,6,8]  
ghci> sort "This will be sorted soon"  
"    Tbdeehiillnooorssstw"  

ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]  

ghci> inits "w00t"  
["","w","w0","w00","w00t"]  
ghci> tails "w00t"  
["w00t","00t","0t","t",""]  
ghci> let w = "w00t" in zip (inits w) (tails w)  
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]  

-}


-- Let's use a fold to implement searching a list for a sublist.
-- search :: (Eq a) => [a] -> [a] -> Bool  
-- search needle haystack =   
--     let nlen = length needle  
--     in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)  

{-

ghci> "cat" `isInfixOf` "im a cat burglar"  
True  
ghci> "Cat" `isInfixOf` "im a cat burglar"  
False  
ghci> "cats" `isInfixOf` "im a cat burglar"  
False  

ghci> "hey" `isPrefixOf` "hey there!"  
True  
ghci> "hey" `isPrefixOf` "oh hey there!"  
False  
ghci> "there!" `isSuffixOf` "oh hey there!"  
True  
ghci> "there!" `isSuffixOf` "oh hey there"  
False  


elem and notElem check if an element is or isn't inside a list.

partition takes a list and a predicate and returns a pair of lists. The first list in the result contains all the elements that satisfy the predicate, the second contains all the ones that don't.

ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
("BOBMORGAN","sidneyeddy")  
ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]  
([5,6,7],[1,3,3,2,1,0,3])  


find takes a list and a predicate and returns the first element that satisfies the predicate. But it returns that element wrapped in a Maybe value. We'll be covering algebraic data types more in depth in the next chapter but for now, this is what you need to know: a Maybe value can either be Just something or Nothing. Much like a list can be either an empty list or a list with some elements, a Maybe value can be either no elements or a single element. And like the type of a list of, say, integers is [Int], the type of maybe having an integer is Maybe Int. Anyway, let's take our find function for a spin.

ghci> find (>4) [1,2,3,4,5,6]  
Just 5  
ghci> find (>9) [1,2,3,4,5,6]  
Nothing  
ghci> :t find  
find :: (a -> Bool) -> [a] -> Maybe a  

elemIndex is kind of like elem, only it doesn't return a boolean value. It maybe returns the index of the element we're looking for. If that element isn't in our list, it returns a Nothing.

ghci> :t elemIndex  
elemIndex :: (Eq a) => a -> [a] -> Maybe Int  
ghci> 4 `elemIndex` [1,2,3,4,5,6]  
Just 3  
ghci> 10 `elemIndex` [1,2,3,4,5,6]  
Nothing  

elemIndices is like elemIndex, only it returns a list of indices, in case the element we're looking for crops up in our list several times. Because we're using a list to represent the indices, we don't need a Maybe type, because failure can be represented as the empty list, which is very much synonymous to Nothing.

ghci> ' ' `elemIndices` "Where are the spaces?"  
[5,9,13]  

findIndex is like find, but it maybe returns the index of the first element that satisfies the predicate. findIndices returns the indices of all elements that satisfy the predicate in the form of a list.

ghci> findIndex (==4) [5,3,2,1,6,4]  
Just 5  
ghci> findIndex (==7) [5,3,2,1,6,4]  
Nothing  
ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"  
[0,6,10,14]  

We already covered zip and zipWith. We noted that they zip together two lists, either in a tuple or with a binary function (meaning such a function that takes two parameters). But what if we want to zip together three lists? Or zip three lists with a function that takes three parameters? Well, for that, we have zip3, zip4, etc. and zipWith3, zipWith4, etc. These variants go up to 7. While this may look like a hack, it works out pretty fine, because there aren't many times when you want to zip 8 lists together. There's also a very clever way for zipping infinite numbers of lists, but we're not advanced enough to cover that just yet.
ghci> zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]  
[7,9,8]  
ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]  
[(2,2,5,2),(3,2,5,2),(3,2,3,2)]  


lines is a useful function when dealing with files or input from somewhere. It takes a string and returns every line of that string in a separate list.
ghci> lines "first line\nsecond line\nthird line"  
["first line","second line","third line"] 

unlines is the inverse function of lines. It takes a list of strings and joins them together using a '\n'.
ghci> unlines ["first line", "second line", "third line"]  
"first line\nsecond line\nthird line\n"  

words and unwords are for splitting a line of text into words or joining a list of words into a text. Very useful.
ghci> words "hey these are the words in this sentence"  
["hey","these","are","the","words","in","this","sentence"]  
ghci> words "hey these           are    the words in this\nsentence"  
["hey","these","are","the","words","in","this","sentence"]  
ghci> unwords ["hey","there","mate"]  
"hey there mate"  

delete takes an element and a list and deletes the first occurence of that element in the list.
ghci> delete 'h' "hey there ghang!"  
"ey there ghang!"  
ghci> delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere ghang!"  
ghci> delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere gang!"  

\\ is the list difference function. It acts like a set difference, basically. For every element in the right-hand list, it removes a matching element in the left one.
ghci> [1..10] \\ [2,5,9]  
[1,3,4,6,7,8,10]  
ghci> "Im a big baby" \\ "big"  
"Im a  baby"  

union also acts like a function on sets. It returns the union of two lists. It pretty much goes over every element in the second list and appends it to the first one if it isn't already in yet. Watch out though, duplicates are removed from the second list!
ghci> "hey man" `union` "man what's up"  
"hey manwt'sup"  
ghci> [1..7] `union` [5..10]  
[1,2,3,4,5,6,7,8,9,10]  

intersect works like set intersection. It returns only the elements that are found in both lists.
ghci> [1..7] `intersect` [5..10]  
[5,6,7]  

insert takes an element and a list of elements that can be sorted and inserts it into the last position where it's still less than or equal to the next element. In other words, insert will start at the beginning of the list and then keep going until it finds an element that's equal to or greater than the element that we're inserting and it will insert it just before the element.
ghci> insert 4 [3,5,1,2,8,2]  
[3,4,5,1,2,8,2]  
ghci> insert 4 [1,3,4,4,1]  
[1,3,4,4,4,1]  
-}