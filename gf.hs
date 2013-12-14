import Data.Bits
import Data.List

ppW8, ppW16:: Integer --primitive polynomial in GF(8), GF(16), GF(32), respectively.
ppW8 = 285 --x^8+x^4+x^3+x^2+1, [1 0001 1101]
ppW16 = 69643 -- x^16+x^12+x^3+x^1+1

gfpolynomial :: Integer -> Integer --get polynomial of w
gfpolynomial w = case w of
	16 -> ppW16
	8 -> ppW8					

gfilogList :: Integer -> Integer -> [Integer]--get infinite gf elements if i is infinite
gfilogList w i 
	| i == 0 	= [1]
	| i > 0 	= do
		let xs = gfilogList w (i-1)
		let j = (last xs) `shift` 1
		if (j .&. 2^w) /= 0
			then do xs ++ [(j `xor` (gfpolynomial w))]
			else do xs ++ [j]

gfilogtable :: Integer -> [Integer] --Generated element set: gfilogtable[i] = a, a = x^i
gfilogtable w = gfilogList w (2^w-2)

gfelem :: Integer -> Int -> Integer -- Generated element: gfelem w i = a, where a = x^i
gfelem w i = (gfilogtable w) !! i

gfmul :: Integer -> Integer -> Integer -> Integer --multiply operation in GF
gfmul a b w
	| a == 0 	= 0
	| b == 0 	= 0
	| otherwise	= do
		let ilogtable = gfilogtable w
		let ia = head (elemIndices a ilogtable)
		let ib = head (elemIndices b ilogtable)
		ilogtable !! ((ia + ib) `mod` (2^w-1));

gfdiv :: Integer -> Integer -> Integer -> Integer --multiply operation in GF
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

