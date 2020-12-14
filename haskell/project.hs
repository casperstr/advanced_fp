
import Control.Monad
import Control.Exception

data Cell = X | O | Empty
    deriving (Show)

data WrongColumnException = WrongColumnException
  deriving (Show)
instance Exception WrongColumnException

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
    if columns /= length str then throwIO WrongColumnException
    else return $ map parseCharacter str

parseBoard :: (Int, Int) -> IO [[Cell]]
parseBoard (rows, columns) = do
    lines <- replicateM rows getLine
    mapM (parseLine columns) lines

     
main :: IO ()
main = do
    settings <- getLine
    board <- parseBoard (parseSettings settings)
    print board
