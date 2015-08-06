-- 2 - STARTING OUT

	-- the basics

	5 == 4 		-- 5 equals 4
	5 /= 4 		-- 5 does not equal 4
	"hello" 	-- double quotes mean string
	'h' 		-- single quotes mean character

	-- general functions

	succ 4		-- get the next item (4)
	min 5 6		-- return the minimum (5)
	max 5 6		-- return the maximum (6)
	div 92 10	-- integral division (10)
	92 `div` 10 -- backticks make an inflix function
	10 `mod` 2 	-- caculate division remainder

	-- the making of a function

	doubleMe x = x * 2	-- this is how you define a function
						-- simple functions are combined into more complex ones
						-- ":l filename" to load a script
						-- ":r" to reload changes

	-- if statement

	doubleWithIf x = if x > 100		-- all IF statements need an ELSE clause
		then x
		else x*2

	-- lists

	numbers = [3,4,4,3,3,2,2,2] -- lists can only contain one type
	[1,2,3] ++ [4,5,6] 			-- "++" is the operator that combines lists ([1,2,3,4,5,6])
	1:[2,3]						-- ":" is the operator that adds an element to the front of a list ([1,2,3])
	[1,2,3] !! 0				-- "!!" is the operator to get the nth element in a list (1)
	head [1,2,3]				-- get the first item in a list (1)
	tail [1,2,3]				-- get all but the first item in list ([2,3])
	last [1,2,3]				-- get the last item in a list (3)
	init [1,2,3]				-- get all but the last item in list ([1,2])
	length [1,2,3]				-- pretty self explanatory
	null []						-- check if a list is empty
	reverse [1,2,3]				-- pretty self explanatory
	take 3 [1,2,3,4,5]			-- takes an number of elemrnts from the beginning of the list ([1,2,3])
	drop 3 [1,2,3,4,5]			-- removes an number of elemrnts from the beginning of the list ([4,5])
	maximum [1,2,3]				-- returns the biggest element in a list (3)
	minimum [1,2,3]				-- returns the smallest element in a list (1)
	sum [1,2,3]					-- pretty self explanatory
	product [1,2,3]				-- pretty self explanatory
	4 `elem` [1,2,3,4]			-- check if a thing is contained in a list
	cycle [1,2,3]				-- cycles a list into an infinite list
	repeat 0					-- like cycle but with one element

	-- ranges

	[1..20]			-- a list from 1 to 20
	['a'..'z']		-- a list from a to z
	[2,4..20]		-- a list from 2 to 20 with a step of 2

	-- list comprehensions

	[ x*2 | x <- [1..10], x `mod` 2 == 0]		-- [ define the variables | set the domain , clause]

	-- tuples

	(1,2,3)						-- this is a tuple
	('a',1)						-- tuples don't have to be homogenous
	fst (1,2)					-- returns the first item in a pair
	snd (1,2)					-- returns the second item in a pair
	zip [1,2,3] ['a','b','c'] 	-- combines two lists in a list of tuples ([(1,'a'),(2,'b'),(3,'c')])

-- 3 - TYPES AND TYPECLASSES

	-- types

	'a' :: Char 					-- character
	"hello" :: [Char]				-- list of characters
	True :: Bool					-- boolean
	(True,'a') :: (Bool,Char)		-- tuple composead of a boolean and a character
	1 :: Int 						-- bounded integer
	1 :: Integer 					-- unbounded integer
	1.0 :: Float					-- floating point with single precision
	1.0 :: Double 					-- floating point with double precision

	-- types of functions

	removeNonUppercase :: [Char] -> [Char] 		-- it's good practice to define the types of a function
	removeNonUppercase st = [ x | x <- st , x `elem` ['A'..'Z']]
	addThree :: Int -> Int -> Int -> Int 		-- add three integers together 
	addThree x y z = x + y + z

	-- type variables

	head :: [a] -> a 		-- "a" in this case is a type variable

	-- typeclasses

	(*) :: Num a => a -> a -> a 		-- the operator "*" takes two parameters that conform to the constrain "Num" and returns them multiplied

	Eq 			-- types that support equality testing
	Ord			-- types that support ordering
	Show		-- types that support being presented as strings ( show 4 )
	Read 		-- types that support being parsed from a string ( read "4" :: Int )
	Enum		-- types that support being sequentially ordered
	Bounded 	-- types that have an upper and lower bound
	Num 		-- types that support acting like numbers
	Integral 	-- Int and Integer
	Floating 	-- Float and Double
	RealFloat

-- 4 - SYNTX IN FUNCTIONS

	-- pattern matching

	isSmall :: (Integral a) => a -> String 			-- the patterns are checked from top to bottom
	isSmall 1 = "one"								-- first the type is defined
	isSmall 2 = "two"
	isSmall 3 = "three"
	isSmall x = "too big" 

	fact :: (Integral a) => a -> a 					-- here's another example
	fact 0 = 1
	fact n = n * fact (n - 1)

	first :: (a,b,c) -> a 							-- you can use pattern matching with tuples
	first (x,_,_) = x

	[ a+b | (a,b) <- list ]							-- and in list comprehensions

	head' :: [a] -> a 								-- and in functions involving lists
	head' (x:_) = a

	firstLetter :: String -> String 				-- the "@" sign give a name to the whole thing while checking it has at least one element
	firstLetter "" = "Empty string"
	firstLetter full@(x:_) = "The first letter of " ++ full ++ " is " ++ [x]

	-- guards

	bmiTell :: (RealFloat a) => a -> a -> String 							-- a guard act like a tree of IF statements
	bmiTell w h
		| value <= 18.5 = "You should eat a bit more"						-- when a guard results False, the next one is checked
		| value <= 25.0 = "You're too normal for us"
		| value <= 30.0 = "Get a bit less ice-cream next time"
		| otherwise = "You are beyond any human possibility of reproach"	-- "otherwise" by definition returns True, so it takes everything in
		where value = w / h ^ 2 											-- in the WHERE section we can define variables to use throughout the guards

	calcBmis :: (RealFloat a) => [(a,a)] -> [a]								
	calcBmis xs = [ bmi w h | (w,h) <- xs]
		where bmi weight height = weight / height ^ 2 						-- in the where section also functions can be defined

	-- let bindings

	4 * (let a = 9 in a + 1) + 2 			-- let bindings let you define very local variables to be used in the "IN" part
	4 * (let a = 9; b = 4 in a + b) + 2 	-- to define more variables you can go with semicolons

	cyl :: (RealFloat a) => a -> a -> a
	cyl r h =
		let sideArea = h * 2 * r * pi 		-- or you can go inline
			topArea = r^2 * pi
		in 	sideArea + 2 * topArea

	calcBmis :: (RealFloat a) => [(a,a)] -> [a]
	calcBmis xs = [ bmi | (w,h) <- xs, let bmi = w/h^2, bmi >= 25.0]	-- or you can put them in list comprehensions

	-- case experssions ( switch )

	describeList :: [a] -> String 												-- case expressions are sintactical sugar for pattern matching
	describeList xs = "The list is " ++ case xs of 
												[] -> "empty." 
												[x] -> "a singleton list."
												xs -> "a longer list."

-- 6 - RECURSION
	
	maximum' :: (Ord a) => [a] -> a 								-- this is an example of recursion
	maximum' [] = error "There can be no maximum to an empty list"
	maximum' [x] = x
	maximum (x:xs)
		| x > maxTail = x
		| otherwise = maxTail
		where maxTail = maximum' xs

	maximum' (x:xs) = max x (maximum' xs) -- it could be written more elegantly this way

	replicate' :: (Integral a) => a -> b -> [b] -- makes a list containing x n times
	replicate' n x
		| n <= 0 = []
		| otherwise = x:replicate' (n-1) x

	take' :: (Integral a) => a -> [b] -> [b] -- takes the first n numbers from a list
	take' n _
		| n <= 0 = []
	take' _ [] = []
	take' n (x:xs) = x : take' (n-1) xs  

	reverse' :: [a] -> [a] -- takes list and returnse it reversed
	reverse' [] = []
	reverse' (x:xs) = reverse' xs ++ [x]

	quicksort :: (Ord a) => [a] -> [a]	-- quicksort divides the list into the elements that are smaller than the head and the elements that are bigger than it
	quicksort [] = []
	quicksort (x:xs) =
		let 
		bgr = [ a | a <- xs, a > x]
		smlr = [ a | a <- xs, a <= x]
		in quicksort smlr ++ [x] ++ quicksort bgr

-- 7 - HIGHER ORDER FUNCTIONS

	-- curried functions

	max 4 5			-- every function only takes one parameter; multiple parameters are obtained returning a function from the first function which in turn takes a second parameter and returns a value
	(max 4) 5 		-- that's why the previous two calls are equivalent

	multThree x y z = x * y * z 
	multTwoWithNine = multThree 9  
	multTwoWithNine 2 3 			-- this comes in handy to create partial functions on the fly

	applyTwice :: (a -> a) -> a -> a 	-- functions can be used both as return values and as parameters
	applyTwice f x = f (f x)

	applyTwice (++ " HAHA") "HEY"

	zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 	-- zipWith takes a function and two lists as parameters and returns a third joint one, applying first the function on it
	zipWith' _ [] _ = []
	zipWith' _ _ [] = []
	zipWith' f x:xs y:ys = f x y : zipWith' f xs ys

	zipWith' (+) [4,2,5,6] [2,6,2,3]

	-- maps and filters

	map' :: ( a -> b ) -> [a] -> [b] -- map takes a function and a list and returns another list, with the function applied to every element
	map' _ [] = []
	map' f (x:xs) = f x : map' f xs

	filter' :: ( a -> Bool ) -> [a] -> [a] -- filter takes a function and a list, and returns only the elements that conform to the predicate
	filter' _ [] = []
	filter' p (x:xs)
		| p x = x : filter' p xs
		| otherwise = filter' p xs

	takeWhile (<100) [1..] 		-- takeWhile goes through a list and stops when the predicate returns False, returning the list up to that point
	takeWhile' :: ( a -> Bool) -> [a] -> [a]
	takeWhile' _ [] = []
	takeWhile' p x:xs
		| p x = x : takeWhile' p xs
		| otherwise = []

	-- lambdas

	chain :: (Integral a) => a -> [a]
	chain 1 = [1]
	chain n
		| even n = n:chain (n `div` 2)
		| otherwise = n:chain (n*3+1) 

	num_chains = length (filter (\ xs -> length xs > 15 ) (map (chain) [1..100])) 	-- lambdas are functions without a name, preceded by the character "\" and surrounded by parentheses to limit the scope
																					-- the parameters are separated by "->" from the function body

	zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5] 		-- they can take any number of parameters

	-- folding functions

	foldl (\acc x -> acc + x) 0 [1,2,3] 	-- foldl takes a two parameter function, of which the first is the accumulator, then a starting value for it and a list to go through while applying the function to every element
	foldr (\x acc -> acc + x) 0 [1,2,3]		-- foldr walks the list from the right and has the accumulator parameter on the right
											-- because ":" is less expensive then "++", we use foldr when building a list from another list
	foldl1 (+) [1,2,3]						-- flodr 1 takes the first element as the starting accumulator value

	scanl (\acc x -> ax + x) 0 [1,2,3]		-- scanl is similar to foldl, but registers every state of the accumulator in a list

	-- function application with $

	sum $ map sqrt [1..100]		-- the "$" operator has the lowest precedence over anything, and therefore saves us precious keystrokes because we don't have to write lots of parentheses

	-- function composition

	map (negate . abs) [1,-2,7,3,3,2,1]	-- the "." operator composes two functions together, executing the one on the right and then passing the result to the one on the left as a parameter
	sum . replicate 5 . max 6.7 $ 8.9 	-- when using functions with more than one parameter, to be combined they just need to be filled up to one parameter remaining

-- 8 - MODULES

	import Data.List 				-- to import a module, it needs to be at the top of the file 
	import Data.List (nub, sort)	-- to selectively import some functions, include them in parentheses
	import Data.List hiding (nub)	-- or import all of them except some
	import qualified Data.Map as M  -- the qualified import prevents name clashes, prepending to each offender what is defined in as (M.filter)
	
	-- Data.List module

	intersperse 1 [2,3,4] 						-- takes an element and a list and returns another list with the element put between every member of the list ([2,1,3,1,4])
	intercalate " "["hello","there","poople"]	-- takes a list and a list of lists, puts the list between every child list and flattens the result ("hello there poople")
	transpose [[1,2,3],[4,5,6],[7,8,9]]			-- thinking of the list of lists as a 2D matrix, transpose flips the coordinates
	foldl1'										-- is the strict counterpart to the lazy foldl1, therefore it computes all values when asked
	concat [[1,2,3],[4,5,6]]					-- takes a list of lists and flattens it
	concatMap (replicate 2) [1..3]				-- maps a function to a list and then concatenates it ([1,1,2,2,3,3])

	and $ map (==3) [1,3,4]						-- and returns true only if every item in  list is true
	or $ map (==3) [1,3,4]						-- or returns true if one item in a list is true
	any (==3) [1,2,3]							-- checks if any of the elements in a list satisfy a predicate
	all (==3) [1,2,3]							-- checks if all of the elements in a list satisfy a predicate
	iterate (*5) 1 								-- iterate takes a function and a starting value, and then applies the function to the result of the same function, returning all the results in the form of an infinite list
	splitAt 3 [1,2,3,4,5,6]						-- takes a number and a list, and splits the list to the n return the two halves in a tuple
	span (/= " ") "hello poople"				-- takes a predicate and a list, then returns a tuple of lists split when the predicate returns false
	break (== " ") "hello poople"				-- like span, but returns when the predicate is first true
	sort [1,2,3] 								-- gues this is pretty self explanatory
	group [1,1,2,2,3,4,4,3]						-- takes a list and groups adjacent objects if they're equal
	inits "hello"								-- takes a list and applies init recursively (["","h","he","hel","hell","hello"])
	tails "hello"								-- as above but from the end (["","o","lo","llo","ello","hello"])
	"hello" `isInfixOf` "hello poople"			-- takes two lists and checks if te sublist is contained in the list
	"hello" `isPrefixOf` "hello poople"			-- checks if the sublist is at the start of the list
	"poople" `isSuffixOf` "hello poople"		-- checks if the sublist is at the end of the list
	partition (>3) [1,2,3,4,5,6,7] 				-- takes a predicate and a list and returns atuple of two lists, the first contains the elements that satisfy the predicate, and the other the ones that don't
	find (==2) [1,2,3]							-- find takes a predicate and a list, and returns the first element that satisfies the predicate, wrapped in a maybe value
	4 `elemIndex` [1,2,3,4,5]					-- elemIndex returns the index of an element if it belongs in the list, otherwise it returns Nothing
	' ' `elemIndices` "hello poople, you there?"-- returns a list of indices where the element is found in the list
	findIndex (>4) [3,4,5,6]					-- like find, but returns the index					
	findIndices (>4) [3,4,5,6]					-- like find, but returns a list of indices
	lines "hello poople,\nyou there?"			-- returns a list of strings, divided by line
	unlines ["hello poople,","are you there?"]	-- basically the opposite of lines
	words "hello poople, you there?"			-- divide a string by whitespace
	unwords ["hello","poople","you","there"]	-- joins a list of word with whitespace
	delete 'h' "hello poople" 					-- deletes the first occurance of an element
	[1,2,3,4] \\ [2,3]							-- "\\" is the operand for list difference ([1,4])
	[1,2,3] `union` [3,4,5]						-- adds two lists togethter but not including diplicates ([1,2,3,4,5])
	[1,2,3] `intersect` [2,3,4]					-- returns only the elements in both lists
	insert 4 [1,2,3,5,6]						-- takes an element and a list and inserts it to the left of the first element that is >= to it
	groupBy ((==) `on` (> 0)) [-1,-2,1,2]		-- on takes a two parameter function and a one parameter function, and returns a predicate function with two parameters, x and y, which are plugged in the second function before being put as parameters of the first one
	sortBy (compare `on` length) ["ab","","a"]	-- sortBy takes a two parameter function and a list and sorts it accordingly

	-- Data.Char module

					-- all the following functions are type :: Char -> Bool
	all isAlphaNum "hello poople" 	-- combine these functions with "all" to check on a whole list/string (True)

	isControl 		-- ('\a','\n','\t' ecc)
	isSpace			-- (' ')
	isLower			-- (a..z)
	isUpper			-- (A..Z)
	isAlpha			-- (a..z ++ A..Z)
	isAlphaNum		-- (a..z ++ A..Z ++ 0..9)
	isPrint			-- checks if character is printable
	isDigit			-- (0..9)
	isOctDigit 		-- (0..7)
	isHexDigit		-- (0..9 ++ a..f ++ A..F)
	isLetter		-- (a..z ++ A..Z)
	isNumber		-- anything in the unicode number category (fractions ecc)
	isPunctuation
	isSymbol		-- mathematical anc currency symbols
	isAscii 		-- checks if a acharacter belongs to the ascii subset
	digitToInt		-- (1..9 ++ a..f ++ A..F)
	ord 			-- returns the ascii value of a character
	chr 			-- returns the character for an ascii value

	-- Data.Map module

	import qualified Data.Map as Map 		-- some functions of this module clash with Prelude
	fromList [(1,"hallo"),(2,"poople")]		-- turns an association list into a Map, discrding duplicate keys
	empty 									-- returns a n empty map
	insert 1 "hello" Map.empty 				-- takes a key, a value and a map and inserts the pair in the map
	null 									-- checks if a map is empty
	size 									-- returns the size of a map
	lookup 1 								-- returns the value stored for a key, or nothing if the key is not in store
	member 1 Map.empty 						-- takes a key and a map and returns True if the Mao has the kay in store
	map 									-- these two should
	filter 									-- work as expected
	toList 									-- the inverse of fromList
	keys 									-- returns a list of keys
	elems 									-- returns a list of elements
	fromListWith (++) [(1,"hello"),(2,"poople"),(1,"hey")] -- takes a function and an association list and executes the function on duplicates (same key)
	insertWith (++) 1 "poople" Map.empty 	-- inserts a key and a values, and if it finds a conflict it executes the function

	-- Data.Set module

	import qualified Data.Set as Set
	fromList 		-- converts a list into a set, and therefore weeds out duplicates
	intersection	-- returns the elements in both sets
	difference
	union
	null
	size
	member
	empty
	insert
	delete
	isSubsetOf  		-- checks if all the elements of the first set are contained in the second
	isProperSubsetOf	-- checks if all the elements of the first set are contained in the second but the second has more elements
	map
	filter
	toList 		-- coonverting to a set and then back to a list is faster then using nub

	-- custom modules

		-- this is the module "Geometry.hs"
	module Geometry
	( cubeArea 		-- these functions are the ones accessible to the world
	, cubeVolume
	, cuboidArea
	, cuboidVolume
	) where

	cubeVolume :: Float -> Float  
	cubeVolume side = cuboidVolume side side side  
	  
	cubeArea :: Float -> Float  
	cubeArea side = cuboidArea side side side  
	  
	cuboidVolume :: Float -> Float -> Float -> Float  
	cuboidVolume a b c = rectangleArea a b * c  
	  
	cuboidArea :: Float -> Float -> Float -> Float  
	cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
	  
	rectangleArea :: Float -> Float -> Float  
	rectangleArea a b = a * b 					-- some helper functions are not exported

		-- modules can also have submodules
		-- inside a folder called "Geometry" (mind the capital G) we put "Cuboid.hs"

	module Geometry.Cuboid  
	( volume  
	, area  
	) where  
	  
	volume :: Float -> Float -> Float -> Float  
	volume a b c = rectangleArea a b * c  
	  
	area :: Float -> Float -> Float -> Float  
	area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
	  
	rectangleArea :: Float -> Float -> Float  
	rectangleArea a b = a * b

		-- and "Cube.hs"

	module Geometry.Cube  
	( volume  
	, area  
	) where  
	  
	import qualified Geometry.Cuboid as Cuboid  
	  
	volume :: Float -> Float  
	volume side = Cuboid.volume side side side  
	  
	area :: Float -> Float  
	area side = Cuboid.area side side side	




