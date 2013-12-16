import Data.Bits
import Data.List
import System.Random
--import System.IO

ppW8, ppW16:: Int --primitive polynomial in GF(8), GF(16), GF(32), respectively.
ppW8 = 285 --x^8+x^4+x^3+x^2+1, [1 0001 1101]
ppW16 = 69643 -- x^16+x^12+x^3+x^1+1

test :: [a] -> Int
test xs = len
	where  len = length xs

sublist :: [a] -> Int -> Int -> [a]
sublist xs beg len -- [beg, end)
	| beg < 0 				= []
	| beg + len > length xs = drop beg xs
	| otherwise 			= take len (drop beg xs)

omitOneIndex :: [a] -> Int -> [a]
omitOneIndex xs i = (take i xs) ++ (drop (i+1) xs)

omitListIndexs :: [a] -> [Int] -> [a]
omitListIndexs xs indexs = [(xs !! i)| i <- remainIndex xs indexs]
	where remainIndex xs indexs = (take (length xs)[0..]) \\ indexs


chgOneIndexWithValue :: [a] -> Int -> a -> [a]
chgOneIndexWithValue xs i x = (take i xs) ++ [x] ++ (drop (i+1) xs)

gfpolynomial :: Int -> Int --get polynomial of w
gfpolynomial w = case w of
	16 -> ppW16
	8 -> ppW8					

gfilogList :: Int -> Int -> [Int]--get infinite gf elements if i is infinite
gfilogList i w
	| i == 0 	= []
	| i == 1 	= [1]
	| i > 1 	= do
		let xs = gfilogList (i-1) w
		let j = (last xs) `shift` 1
		if (j .&. 2^w) /= 0
			then do xs ++ [(j `xor` (gfpolynomial w))]
			else do xs ++ [j]

gfilogtable :: Int -> [Int] --Generated element set: gfilogtable[i] = a, a = x^i
gfilogtable w = gfilogList (2^w-1) w

gfelem :: Int -> Int -> Int -- Generated element: gfelem w i = a, where a = x^i
gfelem w i = (gfilogtable w) !! i

gfmul :: Int -> Int -> Int -> Int --multiply operation in GF
gfmul a b w
	| a == 0 	= 0
	| b == 0 	= 0
	| otherwise	= do
		let ilogtable = gfilogtable w
		let ia = head (elemIndices a ilogtable)
		let ib = head (elemIndices b ilogtable)
		ilogtable !! ((ia + ib) `mod` (2^w-1));

gfdiv :: Int -> Int -> Int -> Int --multiply operation in GF
gfdiv a b w
	| a == 0 	= 0
	| b == 0 	= -1
	| otherwise = do
		let ilogtable = gfilogtable w
		let ia = head (elemIndices a ilogtable)
		let ib = head (elemIndices b ilogtable)
		if ia - ib < 0
			then do ilogtable !! (ia - ib + 2^w-1)
			else do ilogtable !! (ia - ib)

listxor :: [Int] -> Int ---P disk value
listxor [] = -1
listxor [x] = x
listxor (x:xs) = x `xor` (listxor xs) 

raid5 :: [Int] -> [Int] -- xs ++ P
raid5 xs = xs ++ [(listxor xs)]

rsmulist :: [Int] -> Int -> [Int] -- the list of parameters of Q
rsmulist xs w = do 
	zipWith (gfmulw) (gfilogList (length xs) w) xs
		where gfmulw a b = gfmul a b w

rs :: [Int] -> Int -> Int -- Q disk value
rs xs w = listxor (rsmulist xs w)

raid6 :: [Int] -> Int ->  [Int] ---xs ++ P ++ Q
raid6 xs w = xs ++ [(listxor xs), (rs xs w)]

--faildisks :: [Int] -> [Int] -> [Int]
--faildisks disks [] = disks
--faildisks disks [x] = 
--faildisks disks

--raid6_1_D :: [Int] -> Int -> Int
--raid6FailOneDataDisk xs i = do --- xs is the original disks values, and i is the failed disk id, recovery i's value
--	let front = take i xs
--	let rest = drop (i+1) xs
--	let back = init rest
--	listxor (front ++ back)

raid6OneD :: [Int] -> Int -> [Int]
raid6OneD xs i = do
	let front = take i xs
	let rest = drop (i+1) xs
	let back = init rest
	front ++ [listxor (front ++ back)] ++ rest

raid6OneP :: [Int] -> Int -> [Int]
raid6OneP xs i = raid6OneD xs i

raid6OneQ :: [Int] -> Int -> [Int]
raid6OneQ xs w = init xs ++ [(rs front w)]
	where front = take ((length xs) - 2) xs

--raid6TwoDD :: [Int] -> [Int] ->Int -> Int
--raid6TwoDD xs ddindexs w = do
--	let p' = listxor (omitListIndexs xs ddindexs)




raid6TwoDP :: [Int] -> Int -> Int -> [Int]
raid6TwoDP xs dindex w = do
	let len = length xs
	let ddisks = take (len-2) xs
	let qlist' = zipWith3 (gfmul) (omitOneIndex ddisks dindex) (omitOneIndex (gfilogList (len-2) w) dindex) (repeat w)
	let d = gfmul (gfdiv 1 ((gfilogtable w)!!dindex) w) ((last xs) `xor` (listxor qlist')) w
	let newddisks = chgOneIndexWithValue ddisks dindex d
	let p = listxor newddisks
	newddisks ++ [p] ++ [last (xs)]

raid6TwoDQ :: [Int] -> Int -> Int -> [Int]
raid6TwoDQ xs dindex w = raid6OneQ dq w
	 where dq = raid6OneD xs dindex

raid6TwoPQ :: [Int] -> Int -> [Int]
raid6TwoPQ xs w = do
	let len = length xs
	let ddisks = take (len - 2) xs
	ddisks ++ [listxor ddisks] ++ [rs ddisks w]


raid6recv :: [Int] -> [Int] --- [...-1...] -> [......], recovery whose value is -1(failed)
raid6recv xs 
	| (length fdisks) > 2 	= xs
	| (length fdisks) <=0 	= xs	
	| (length fdisks) == 1 	= [1]
	| (length fdisks) == 2  = [1,2]
	where fdisks = elemIndices (-1) xs

