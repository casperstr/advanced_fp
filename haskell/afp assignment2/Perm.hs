
module Perm (perm, test) where 
import Test.QuickCheck (withMaxSuccess, quickCheck, chooseInt, Gen)
import Data.List



perm :: [Int] -> [Int] -> Bool
perm inp out = isStackPermutation (reverse inp) (reverse out) []

isStackPermutation :: [Int] -> [Int] -> [Int] -> Bool
isStackPermutation [] out stack = out == stack
isStackPermutation (x : xs) y [] = isStackPermutation xs y [x]
isStackPermutation (x : xs) (y : ys) (z : zs)
  | y == z =
    isStackPermutation (x : xs) ys zs
  | otherwise =
    isStackPermutation xs (y : ys) (x : z : zs)

contains231 :: [Int] -> Bool
contains231 [] = False
contains231 xs =
  let max = maximum xs
      a = takeWhile (/= max) xs
      b = dropWhile (/= max) xs
      isCont = not (null a) && not(null b) && maximum a < max && minimum b < maximum a
   in isCont || contains231 [c | c <- xs, c /= max]

prop_contains231:: [Int] -> Bool
prop_contains231 inp = 
  let list = nub inp 
  in perm (sort list) list /= contains231 list

prop_231NeverOrdable:: [Int] -> Gen Bool
prop_231NeverOrdable inp = do
  index <- chooseInt (0, length inp)
  m <- chooseInt (-1000, 1000)
  l <- chooseInt (-2000, m - 1)
  r <- chooseInt (-3000, l - 1)
  return $ 
    let (left,right) = splitAt index inp
        list = left ++ [l,m,r] ++ right
    in not $ perm (sort list) list
  

prop_SameInputOrderable :: [Int] -> Bool
prop_SameInputOrderable inp = perm inp inp

prop_DifferentElementsNotOrdable :: [Int] -> Int -> Bool
prop_DifferentElementsNotOrdable inp x = not $ perm (x:inp) inp

test :: IO ()
test = 
  do 
    putStrLn "Contains 231:"
    quickCheck (withMaxSuccess 10000 prop_contains231)
    putStrLn "Same input"
    quickCheck prop_SameInputOrderable
    putStrLn "Different elements not ordable"
    quickCheck prop_DifferentElementsNotOrdable
    putStrLn "231 Never Ordable"
    quickCheck  (withMaxSuccess 10000 prop_231NeverOrdable)