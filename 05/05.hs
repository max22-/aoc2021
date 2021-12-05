import Parsing
import Control.Monad
import Data.List

type Line = ((Int, Int), (Int, Int))
type Point = (Int, Int)

linep :: Parser Line
linep = do
     x1 <- integer
     token (char ',')
     y1 <- integer
     symbol "->"
     x2 <- integer
     token (char ',')
     y2 <- integer
     return ((x1, y1), (x2, y2))

linesp :: Parser [Line]
linesp = many linep

expand :: Line -> [Point]
expand ((x1, y1), (x2, y2)) | x1 == x2 = [(x1, y) | y <- [(min y1 y2) .. (max y1 y2)]]
expand ((x1, y1), (x2, y2)) | y1 == y2 = [(x, y1) | x <- [(min x1 x2) .. (max x1 x2)]]
expand l = []


solution = length . filter (\(p, c) -> c >= 2) . map (\l -> (head l, length l)) . group .sort . concatMap expand

main :: IO()
main = do
     input <- getContents
     case parse linesp input of
          [([], _)] -> putStrLn "Input error"
          [(ls, "")] -> print $ solution ls
          _ -> putStrLn "Input error"