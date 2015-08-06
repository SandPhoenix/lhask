--doubleMe :: (Int,Float a) -> (Int,Float b)
import Data.Char

doubleMe x = if x > 100
	then x
	else x*2

euler_1 = sum ([ x | x <- [1..999],x `mod` 3 == 0 || x `mod` 5 == 0])
fib l = euler_2 ((l!!((length l)-1) + l!!((length l)-2) ) : l)
euler_2 l = if (last l) < (10**6) then fib l else l

isSmall :: (Integral a) => a -> String
isSmall 1 = "one"
isSmall 2 = "two"
isSmall 3 = "three"
isSmall x = "too big" 

firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter full@(x:_) = "The first letter of " ++ full ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
	| value <= 18.5 = "You should eat a bit more"
	| value <= 25.0 = "You're too normal for us"
	| value <= 30.0 = "Get a bit less ice-cream next time"
	| otherwise = "You are beyond any human possibility of reproach"
	where value = w / h ^ 2

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of 
											[] -> "empty." 
											[x] -> "a singleton list."
											xs -> "a longer list."

replicate' :: (Integral a) => a -> b -> [b]
replicate' n x
	| n <= 0 = []
	| otherwise = x:replicate' (n-1) x

take' :: (Integral a) => a -> [b] -> [b]
take' n _
	| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' i (x:xs)
	| i == x = True
	| otherwise = elem' i xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let 
	bgr = [ a | a <- xs, a > x]
	smlr = [ a | a <- xs, a <= x]
	in quicksort smlr ++ [x] ++ quicksort bgr

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
	let 
	bgr = filter (>x) xs
	smlr = filter (<=x) xs
	in quicksort smlr ++ [x] ++ quicksort bgr


takeWhile' :: ( a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
	| p x = x : takeWhile' p xs
	| otherwise = []

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n = n:chain (n `div` 2)
	| otherwise = n:chain (n*3+1) 

num_chains = length (filter (\ xs -> length xs > 15 ) (map (chain) [1..100]))

needInHay :: (Eq a) => [a] -> [a] -> Bool
needInHay _ [] = False
needInHay needle haystack
	| take nLen haystack == needle = True
	| otherwise = needInHay needle (tail haystack)
	where nLen = length needle

shift :: Int -> String -> String
shift n xs = map (chr . (+n) . ord) xs
 
decode :: Int -> String -> String
decode n xs = shift ( negate n ) xs





