import Data.Bits
import Data.List

ppW8, ppW16:: Int --primitive polynomial in GF(8), GF(16), GF(32), respectively.
ppW8 = 285 --x^8+x^4+x^3+x^2+1, [1 0001 1101]
ppW16 = 69643 -- x^16+x^12+x^3+x^1+1

-- get the sublist of xs from index of 'beg', and the length is 'len'
sublist :: [a] -> Int -> Int -> [a]
sublist xs beg len 
	| beg < 0 				= []
	| beg >= length xs	 	= []
	| beg + len > length xs = drop beg xs
	| otherwise 			= take len (drop beg xs)

-- delete the index'i' in xs
omitOneIndex :: [a] -> Int -> [a] 
omitOneIndex xs i 
	| i < 0 			= xs
	| i >= length xs 	= xs
	| otherwise 		= (take i xs) ++ (drop (i+1) xs) 

-- delete the indexs in xs
omitListIndexs :: [a] -> [Int] -> [a] 
omitListIndexs xs indexs = [(xs !! i)| i <- remainIndex xs (nub indexs)]
	where remainIndex xs indexs = (take (length xs)[0..]) \\ indexs

-- change the index 'i' value of 'v'
chgOneIndexWithValue :: [a] -> Int -> a -> [a]
chgOneIndexWithValue xs i v
	| i < 0 			= xs
	| i >= length xs 	= xs 
	| otherwise			= (take i xs) ++ [v] ++ (drop (i+1) xs)

-- change the indexs '(i:indexs)' values of '(v:values)'
chgIndexsWithValues :: [a] -> [Int] -> [a] -> [a]
chgIndexsWithValues xs [] [] = xs
chgIndexsWithValues xs (i:indexs) (v:values)
	| length (i:indexs) /= length(v:values) 	= xs 
	| otherwise = chgIndexsWithValues xs' indexs values 
		where xs' = chgOneIndexWithValue xs i v

--get polynomial of w
gfpolynomial :: Int -> Int 
gfpolynomial w = case w of
	16 -> ppW16
	8 -> ppW8					

--get infinite gf elements if i is infinite
gfilogList :: Int -> Int -> [Int]
gfilogList i w
	| i <= 0 	= []
	| i == 1 	= [1]
	| otherwise = do
		let xs = gfilogList (i-1) w
		let j = (last xs) `shift` 1
		if (j .&. 2^w) /= 0 then 
			xs ++ [(j `xor` (gfpolynomial w))]
			else xs ++ [j]

--Generated element set: gfilogtable[i] = a, a = x^i
gfilogtable :: Int -> [Int] 
gfilogtable w = gfilogList (2^w-1) w

-- Generated element: gfelem w i = a, where a = x^i
gfelem :: Int -> Int -> Int 
gfelem w i = (gfilogtable w) !! i

--multiply operation in GF
gfmul :: Int -> Int -> Int -> Int 
gfmul a b w
	| a == 0 							= 0
	| b == 0 							= 0
	| a < 2^w, b < 2^w, a > 0, b > 0 	= do
		let ilogtable = gfilogtable w
		let ia = head (elemIndices a ilogtable)
		let ib = head (elemIndices b ilogtable)
		ilogtable !! ((ia + ib) `mod` (2^w-1))
	| otherwise 						= -1

--division operation in GF
gfdiv :: Int -> Int -> Int -> Int 
gfdiv a b w
	| a == 0 							= 0
	| b == 0 							= -1
	| a < 2^w, b < 2^w, a > 0, b > 0 	= do
		let ilogtable = gfilogtable w
		let ia = head (elemIndices a ilogtable)
		let ib = head (elemIndices b ilogtable)
		if ia - ib < 0
			then do ilogtable !! (ia - ib + 2^w-1)
			else do ilogtable !! (ia - ib)
	| otherwise 						= -1

-- P disk value
listxor :: [Int] -> Int 
listxor [] = -1
listxor [x] = x
listxor (x:xs) = x `xor` (listxor xs) 

-- raid5 generation
raid5 :: [Int] -> [Int]
raid5 xs = xs ++ [(listxor xs)]

raid5recv :: [Int] -> [Int]
raid5recv xs = chgOneIndexWithValue xs i (listxor rest)
	where
		len = length xs
		i = head (elemIndices (-1) xs)
		rest = (omitOneIndex xs i)

--the list of parameters of Q
rs :: [Int] -> Int -> Int
rs xs w = listxor (zipWith3 (gfmul) (gfilogList (length xs) w) 
							xs 
							(repeat w))

--raid6 generation
raid6 :: [Int] -> Int ->  [Int] 
raid6 xs w = xs ++ [(listxor xs), (rs xs w)]---xs ++ P ++ Q

--raid6: data disk failed and recover
raid6OneD :: [Int] -> Int -> [Int]
raid6OneD xs i = do
	let front = take i xs
	let rest = drop (i+1) xs
	let back = init rest
	front ++ [listxor (front ++ back)] ++ rest

--raid6: P disk failed and recover
raid6OneP :: [Int] -> Int -> [Int]
raid6OneP xs i = raid6OneD xs i

--raid6: Q disk failed and recover
raid6OneQ :: [Int] -> Int -> [Int]
raid6OneQ xs w = init xs ++ [(rs front w)]
	where front = take ((length xs) - 2) xs

--raid6: recover one failed disk
raid6recvOne :: [Int] -> Int -> Int -> [Int]
raid6recvOne xs i w
	| i < len-2 	= raid6OneD xs i
	| i == len-2  	= raid6OneP xs i
	| i == len-1  	= raid6OneD xs w
	| otherwise 	= xs
		where 
			len = length xs

--raid6: Two data disks failed and recover
raid6TwoDD :: [Int] -> [Int] ->Int -> [Int]
raid6TwoDD xs ddindexs w = do
	let len = length xs
	let ddisks =  omitListIndexs xs [len-2, len-1]
	let	p = xs !! (len-2)
	let	q = xs !! (len-1)
	let	p' = listxor (omitListIndexs ddisks ddindexs)
	let	qlist' = zipWith3 (gfmul) 
						  (omitListIndexs (gfilogList (len-2) w) ddindexs) 
						  (omitListIndexs ddisks ddindexs) 
						  (repeat w)
	let q' = listxor qlist'
	let	g1 = (gfilogtable w)!!(head ddindexs)
	let	g2 = (gfilogtable w)!!(last ddindexs)
	let	d1 = gfmul (gfdiv 1 (g1 `xor` g2) w) 
				   ((gfmul g2 (p `xor` p') w) `xor` q `xor` q') 
				   w
	let	d2 = gfmul (gfdiv 1 (g1 `xor` g2) w) 
			 	   ((gfmul g1 (p `xor` p') w) `xor` q `xor` q') 
			 	   w
	chgIndexsWithValues xs ddindexs ([d1]++[d2])

--raid6: One data disk and P disk failed and recover
raid6TwoDP :: [Int] -> Int -> Int -> [Int]
raid6TwoDP xs dindex w = do
	let len = length xs
	let ddisks = take (len-2) xs
	let qlist' = zipWith3 (gfmul) (omitOneIndex ddisks dindex) 
						  (omitOneIndex (gfilogList (len-2) w) dindex)
						  (repeat w)
	let d = gfmul (gfdiv 1 ((gfilogtable w)!!dindex) w) 
				  ((last xs) `xor` (listxor qlist')) 
				  w
	let newddisks = chgOneIndexWithValue ddisks dindex d
	let p = listxor newddisks
	newddisks ++ [p] ++ [last (xs)]

--raid6: One data disk and Q disk failed and recover
raid6TwoDQ :: [Int] -> Int -> Int -> [Int]
raid6TwoDQ xs dindex w = raid6OneQ dq w
	 where dq = raid6OneD xs dindex

--raid6: P disk and Q disk failed and recover
raid6TwoPQ :: [Int] -> Int -> [Int]
raid6TwoPQ xs w = do
	let len = length xs
	let ddisks = take (len - 2) xs
	ddisks ++ [listxor ddisks] ++ [rs ddisks w]

--raid6: recover two failed disks
raid6recvTwo :: [Int] -> [Int] -> Int -> [Int]
raid6recvTwo xs indexs w
	| index1 < len-2, index2 < len-2 	= raid6TwoDD xs [index1, index2] w
	| index1 < len-2, index2 == len-2	= raid6TwoDP xs index1 w
	| index1 < len-2, index2 == len-1 	= raid6TwoDQ xs index1 w
	| index1 == len-2, index2 == len -1 = raid6TwoPQ xs w
	| otherwise 						= xs
	where
		len = length xs
		index1 = head (sort (nub indexs))
		index2 = last (sort (nub indexs))

--raid6: recover one or two disks
raid6recv :: [Int] -> Int ->[Int] --- [...-1...] -> [......], recovery whose value is -1(failed)
raid6recv xs w = do
	let fdisks = elemIndices (-1) xs
	if (length fdisks) == 1 then
		let i =  head (elemIndices (-1) xs)
		in raid6recvOne xs i w
	else if (length fdisks) == 2 then
		let ix = elemIndices (-1) xs
		in raid6recvTwo xs ix w
	else xs


