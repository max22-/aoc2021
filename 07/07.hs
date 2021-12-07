import Parsing
import Control.Monad

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = liftM2 (:) p (many (sep >> p))

cost ::  [Int] -> Int -> Int
cost v x = sum . map (abs . (x -)) $ v

--input :: [Int]
--input = [16,1,2,0,4,2,7,1,2,14]

range :: [Int] -> [Int]
range xs = [foldr min (head xs) xs .. foldr max (head xs) xs]

costs :: [Int] -> [Int]
costs xs = map (cost xs) (range xs)

solution :: [Int] -> Int
solution xs = fst . foldr1 (\a b -> if fst a < fst b then a else b) $ zip (costs xs) (range xs)

main :: IO ()
main =  do
     input <- getContents
     case parse (sepBy natural (char ',')) input of
         [] -> error "Input error"
         [(l, "")] -> print $ solution l
         _ -> error "Input error"