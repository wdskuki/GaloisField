import Data.Bits
import Data.List
import System.Random
--import System.IO

ppW8, ppW16:: Int --primitive polynomial in GF(8), GF(16), GF(32), respectively.
ppW8 = 285 --x^8+x^4+x^3+x^2+1, [1 0001 1101]
ppW16 = 69643 -- x^16+x^12+x^3+x^1+1

gfpolynomial :: Int -> Int --get polynomial of w
gfpolynomial w = case w of
	16 -> ppW16
	8 -> ppW8					

gfilogList :: Int -> Int -> [Int]--get infinite gf elements if i is infinite
gfilogList w i 
	| i == 0 	= [1]
	| i > 0 	= do
		let xs = gfilogList w (i-1)
		let j = (last xs) `shift` 1
		if (j .&. 2^w) /= 0
			then do xs ++ [(j `xor` (gfpolynomial w))]
			else do xs ++ [j]

gfilogtable :: Int -> [Int] --Generated element set: gfilogtable[i] = a, a = x^i
gfilogtable w = gfilogList w (2^w-2)

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
	zipWith (gfmulw) (gfilogList w (length xs)) xs
		where gfmulw a b = gfmul a b w

rs :: [Int] -> Int -> Int -- Q disk value
rs xs w = listxor (rsmulist xs w)

raid6 :: [Int] -> Int ->  [Int] ---xs ++ P ++ Q
raid6 xs w = xs ++ [(listxor xs), (rs xs w)]

--faildisks :: [Int] -> [Int] -> [Int]
--faildisks disks [] = disks
--faildisks disks [x] = 
--faildisks disks

raid6FailOneDataDisk :: [Int] -> Int -> Int
raid6FailOneDataDisk xs i = do --- xs is the original disks values, and i is the failed disk id, recovery i's value
	let front = take i xs
	let rest = drop (i+1) xs
	let back = init rest
	listxor (front ++ back)

raid6FailOneDataDisk' :: [Int] -> [Int] --- [...-1...] -> [......], recovery whose value is -1(failed)
raid6FailOneDataDisk' xs = do
