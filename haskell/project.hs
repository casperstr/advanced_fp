import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

data Cell = X | O | Empty
  deriving (Eq)

instance Show Cell where
  show X = "X"
  show O = "O"
  show Empty = "."

data WrongColumnException = WrongColumnException
  deriving (Show)

instance Exception WrongColumnException

equalCellNotConsideringEmpty :: (Cell, Cell) -> Bool
equalCellNotConsideringEmpty (Empty, _) = True
equalCellNotConsideringEmpty (_, Empty) = True
equalCellNotConsideringEmpty (a, b) = a == b

equalRowCellNotConsideringEmpty :: [Cell] -> [Cell] -> Bool
equalRowCellNotConsideringEmpty rowa rowb = all equalCellNotConsideringEmpty (zip rowa rowb)

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

countCellTypes :: [Cell] -> (Int, Int)
countCellTypes row = ((length . filter (== X)) row, (length . filter (== O)) row)

fillTriplesHeuristic :: [Cell] -> [Cell]
fillTriplesHeuristic [] = []
fillTriplesHeuristic [a] = [a]
fillTriplesHeuristic [a, b] = [a, b]
fillTriplesHeuristic (a : b : c : rest)
  | a /= Empty && a == b =
    a : fillTriplesHeuristic (b : opposite a : rest)
  | b /= Empty && b == c =
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
    (xs, os) = countCellTypes row
    maxLength = length row `div` 2
    fillWith cellType = map (\cell -> if cell == Empty then cellType else cell)

tryRemainingPlaces :: Cell -> [Cell] -> [[Cell]]
tryRemainingPlaces cellType row = map genRow emptyIndicies
  where
    emptyIndicies = elemIndices Empty row
    genRow index =
      let (left, right) = splitAt index row
       in left ++ [cellType] ++ tail right

fillTripple3Heuristic :: [Cell] -> [Cell]
fillTripple3Heuristic row
  | xs - 1 == maxLength = fromMaybe row $ find verifyTriples $ map fillCompleteRowHeuristic $ tryRemainingPlaces X row
  | os - 1 == maxLength = fromMaybe row $ find verifyTriples $ map fillCompleteRowHeuristic $ tryRemainingPlaces O row
  | otherwise = row
  where
    (xs, os) = countCellTypes row
    maxLength = length row `div` 2

fillHeuristic = fillTriplesHeuristic . fillCompleteRowHeuristic . fillTripple3Heuristic

solveAllHeuristics :: [[Cell]] -> [[Cell]]
solveAllHeuristics board =
  let solvedRows = map fillHeuristic board
      finalBoard = transpose (map fillHeuristic (transpose solvedRows))
   in if finalBoard == board then finalBoard else solveAllHeuristics finalBoard

replaceFirstEmpty :: Cell -> [[Cell]] -> [[Cell]]
replaceFirstEmpty _ [] = []
replaceFirstEmpty cell (x : xs) =
  let resRow = replaceFirstRowEmpty cell x
   in if resRow /= x then resRow : xs else x : replaceFirstEmpty cell xs

replaceFirstRowEmpty :: Cell -> [Cell] -> [Cell]
replaceFirstRowEmpty _ [] = []
replaceFirstRowEmpty cell (x : xs) = if x == Empty then cell : xs else x : replaceFirstRowEmpty cell xs

solverAux :: [[Cell]] -> Maybe [[Cell]]
solverAux board
  | boardIsDone board && verifyBoard board && verifyUniqueness board = Just board
  | boardIsDone board = Nothing
  | verifyBoard board =
    listToMaybe $
      catMaybes
        [ solve (replaceFirstEmpty O board),
          solve (replaceFirstEmpty X board)
        ]
  | otherwise = Nothing

solve :: [[Cell]] -> Maybe [[Cell]]
solve board
  | boardIsDone board && verifyBoard board && verifyUniqueness board = Just board
  | boardIsDone board = Nothing
  | verifyBoard board = solverAux $ solveAllHeuristics board
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
