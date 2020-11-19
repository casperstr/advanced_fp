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

contains_231 :: [Int] -> Bool
contains_231 [] = False
contains_231 xs =
  let max = maximum xs
      a = takeWhile (/= max) xs
      b = dropWhile (/= max) xs
      isCont = not (null a) && not(null b) && maximum a < max && minimum b < maximum a
   in isCont || contains_231 [c | c <- xs, c /= max]