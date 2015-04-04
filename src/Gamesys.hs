module Gamesys(series, specialNumbers, specialNumbersFromSet) where

import Data.List
import Data.Set as Set hiding (map)


-- Part 1

firstNumber x = ((0.5*x*x)+(30*x)+10)/25

growthRate y f = ((0.02*y)/25)/f

roundToQuarters x = (fromIntegral (round (x*4))) / 4

series x y length 
	| length < 0 = error "length cannot be smaller than zero"
	| length ==0 = fromList []
	| otherwise  = fromList ( map roundToQuarters(f : (map (\r -> g*(f^r)) [1..(length-1)])) )
				where f = firstNumber x
				      g = growthRate y f

-- Part 2

firstSpecial :: Set a -> Maybe a
firstSpecial xs 
	| size xs < 3 = Nothing
	| otherwise = Just (findMax (deleteMax (deleteMax xs)))

approximateDividend = 1000

approximateNumber z = approximateDividend / z

secondSpecial :: (Num a, Ord a) => a -> Set a -> Maybe a
secondSpecial el xs
	| Set.null xs   = Nothing
 	| otherwise = Just (if included then el else elementCloserTo el a b)
			where (a, included, b) = splitMember el xs

specialNumbersFromSet :: Double -> Set Double -> (Maybe Double, Maybe Double)
specialNumbersFromSet 0 _ = error "parameter z cannot be equal to zero"
specialNumbersFromSet z xs = (firstSpecial xs, secondSpecial (approximateNumber z) xs)

specialNumbers :: (Integral a) => Double -> Double -> a -> Double -> (Maybe Double, Maybe Double)
specialNumbers _ _ _ 0 = error "parameter z cannot be equal to zero"
specialNumbers x y length z = (firstSpecial xs, secondSpecial (approximateNumber z) xs)
				where xs = series x y length

elementCloserTo :: (Num a, Ord a) => a -> Set a -> Set a -> a
elementCloserTo number a b 
	| Set.null a = findMin b	-- no need to check 'null a and null b' because a or be contains some elements
	| Set.null b = findMax a
	| otherwise = if abs (number-(findMax a)) < abs (number-(findMin b)) 
				then findMax a
				else findMin b







