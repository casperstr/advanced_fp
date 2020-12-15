import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe

data Cell = X | O | Empty
  deriving (Show, Eq)

data WrongColumnException = WrongColumnException
  deriving (Show)

instance Exception WrongColumnException

verifyTriples :: [Cell] -> Bool
verifyTriples [] = True
verifyTriples [_] = True
verifyTriples [_, _] = True
verifyTriples (a : b : c : xs) = a == Empty || not (a == b && b == c) && verifyTriples (b : c : xs)

verifyEqualLength :: [Cell] -> Bool
verifyEqualLength row =
  let (xs, os) = ((length . filter (== X)) row, (length . filter (== O)) row)
      maxLength = length row `div` 2
   in xs <= maxLength && os <= maxLength

verifyColumnsOrRows :: [[Cell]] -> Bool
verifyColumnsOrRows =
  let verifyRowOrColumn rowOrColumn = verifyTriples rowOrColumn && verifyEqualLength rowOrColumn
   in all verifyRowOrColumn

verifyBoard :: [[Cell]] -> Bool
verifyBoard board =
  let columns = transpose board
   in verifyColumnsOrRows board
        && verifyColumnsOrRows columns

verifyUniqueness :: [[Cell]] -> Bool
verifyUniqueness board =
  let columns = transpose board
   in length (nub board) == length board && length (nub columns) == length columns

boardIsDone :: [[Cell]] -> Bool
boardIsDone = all (notElem Empty)

opposite :: Cell -> Cell
opposite X = O
opposite O = X
opposite Empty = Empty

fillTriplesHeuristic :: [Cell] -> [Cell]
fillTriplesHeuristic [] = []
fillTriplesHeuristic [a] = [a]
fillTriplesHeuristic [a, b] = [a, b]
fillTriplesHeuristic (a : b : c : rest)
  | a == b =
    a : fillTriplesHeuristic (b : opposite a : rest)
  | b == c =
    opposite b : fillTriplesHeuristic (b : c : rest)
  | b == Empty && a == c =
    a : fillTriplesHeuristic (opposite a : c : rest)
  | otherwise = a : fillTriplesHeuristic (b : c : rest)

fillCompleteRowHeuristic :: [Cell] -> [Cell]
fillCompleteRowHeuristic row
  | xs == maxLength = fillWith O row
  | os == maxLength = fillWith X row
  | otherwise = row
  where
    (xs, os) = ((length . filter (== X)) row, (length . filter (== O)) row)
    maxLength = length row `div` 2
    fillWith cellType = map (\cell -> if cell == Empty then cellType else cell)

fillHeuristic = fillTriplesHeuristic . fillCompleteRowHeuristic

solveAllHeuristics :: [[Cell]] -> [[Cell]]
solveAllHeuristics board =
  let solvedRows = map fillHeuristic board
      solvedColumns = transpose (map fillHeuristic (transpose solvedRows))
   in if solvedColumns == board then board else solveAllHeuristics solvedColumns

replaceFirstEmpty :: Cell -> [[Cell]] -> [[Cell]]
replaceFirstEmpty _ [] = []
replaceFirstEmpty cell (x : xs) =
  let resRow = replaceFirstRowEmpty cell x
   in if resRow /= x then resRow : xs else x : replaceFirstEmpty cell xs

replaceFirstRowEmpty :: Cell -> [Cell] -> [Cell]
replaceFirstRowEmpty _ [] = []
replaceFirstRowEmpty cell (x : xs) = if x == Empty then cell : xs else x : replaceFirstRowEmpty cell xs

solve :: [[Cell]] -> Maybe [[Cell]]
solve board
  | boardIsDone board && verifyBoard board && verifyUniqueness board = Just board
  | boardIsDone board = Nothing
  | verifyBoard board =
    let b = solveAllHeuristics board
     in if boardIsDone b && verifyBoard b && verifyUniqueness b
          then Just b
          else
            listToMaybe $
              catMaybes
                [ solve (replaceFirstEmpty O board),
                  solve (replaceFirstEmpty X board)
                ]
  | otherwise = Nothing

parseSettings :: String -> (Int, Int)
parseSettings line =
  let [rows, columns] = words line
   in (read rows, read columns)

parseCharacter :: Char -> Cell
parseCharacter 'X' = X
parseCharacter 'O' = O
parseCharacter '.' = Empty

parseLine :: Int -> String -> IO [Cell]
parseLine columns str = do
  if columns /= length str
    then throwIO WrongColumnException
    else return $ map parseCharacter str

parseBoard :: (Int, Int) -> IO [[Cell]]
parseBoard (rows, columns) = do
  lines <- replicateM rows getLine
  mapM (parseLine columns) lines

printBoard :: Maybe [[Cell]] -> IO ()
printBoard (Just board) = mapM_ (\row -> do mapM_ (putStr . show) row; putStr "\n") board
printBoard Nothing = print "No board found"

main :: IO ()
main = do
  settings <- getLine
  board <- parseBoard (parseSettings settings)
  printBoard (solve board)
