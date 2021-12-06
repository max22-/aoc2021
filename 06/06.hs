import Parsing
import Control.Monad

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = liftM2 (:) p (many (sep >> p))

births :: [Int] -> [Int]
births xs = replicate (length (filter (==0) xs)) 8

dec_reset :: [Int] -> [Int]
dec_reset = map (\x -> if x == 0 then 6 else pred x)

nextgen :: [Int] -> [Int]
nextgen xs = dec_reset xs ++ births xs

solution :: [Int] -> Int
solution = length . last . take (80+1) . iterate nextgen

main :: IO ()
main = do
     input <- getContents
     case parse (sepBy natural (char ',')) input of
	 [] -> error "Input error"
	 [(l, "")] -> print $ solution l
	 _ -> error "Input error"