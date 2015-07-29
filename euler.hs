-- Problem 1
modList :: (Integral a) => a -> [a] -> Bool
modList _ [] = False
modList 0 _ = False
modList n (x:xs)
	| n `mod` x == 0 = True
	| otherwise = modList n xs

euler_1 :: (Integral a) => [a] -> a
euler_1 [] = error "Empty list of divisors"
euler_1 xs = sum [ x | x <- [1..999], modList x xs]

-- Problem 2

fib :: (Integral a) => a -> a -> [a]
fib a b
	| a <= maxNum = fib b (a+b) ++ [a]
	| otherwise = []
	where maxNum = (4 * 10 ^ 6)

euler_2 = sum [ x | x <- (fib 1 2), even x ]

-- problem 3 -- 600851475143

-- isPrime :: (Integral a) => a -> Bool
-- isPrime n 
-- 	| [ x | x <- [2..(n-1)], n `mod` x == 0 ] == [] = True
-- 	| otherwise = False

euler_3 :: (Integral a) => a -> a -> a
euler_3 n d
	| d == n = d
	| n `mod` d == 0 = euler_3 (n `div` d) 2
	| otherwise = euler_3 n (d+1)

-- problem 4

isPal :: (Integral a, Show a) => a -> Bool
isPal n
	| first == (reverse second) = True
	| otherwise = False
	where
	st = show n
	first = take ((length st) `div` 2) st
	second = drop ((length st) `div` 2) st 

euler_4 :: (Integral a, Show a) => a
euler_4 = maximum [ a*b | a<-[999,998..100],b<-[999,998..100], isPal (a*b)]

-- problem 5

isDiv :: (Integral a) => a -> a -> Bool
isDiv d n
	| d == 1 = True
	| n `mod` d /= 0 = False
	| otherwise = isDiv (d-1) n

euler_5 :: (Integral a) => a -> a
euler_5 0 = euler_5 (20)
euler_5 n
	| isDiv 20 n = n
	| otherwise = euler_5 (n+20)

-- problem 6

euler_6 = sum [1..100] ^ 2 - sum [ x ^ 2 | x <- [1..100]]

-- problem 7

-- isPrime :: (Integral a) => a -> a -> Bool
-- isPrime n d
-- 	| d == n = True
-- 	| n `mod` d == 0 = False
-- 	| otherwise = isPrime n (d+1)

euler_7 :: (Integral a) => [a] -> a -> a -> a
euler_7 xs n c
	| c == 10001 = n-1
	| [ x | x <- xs, n `mod` x == 0] == [] = euler_7 (xs ++ [n]) (n+1) (c+1)
	| otherwise = euler_7 xs (n+1) c

-- problem 8

eul_8_num = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

slice :: Int -> Int -> [b] -> [b]
slice from to xs = take (to - from) (drop from xs)
makeList xs i
	| (i + 13) <= length eul_8_num = makeList ((product [ read [x] :: Int | x <- (slice i (i+13) eul_8_num) ]):xs) (i+1)
	| otherwise = xs

euler_8 = maximum (makeList [] 0)

-- problem 9








