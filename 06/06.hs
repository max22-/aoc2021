import Parsing
import Control.Monad

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = liftM2 (:) p (many (sep >> p))

-- Lanternfishes
--  0  1  2  3  4  6  7  8     <- counters (index of the list)
-- [0, 1, 1, 2, 1, 0, 0, 0]    <- how many have this counter value

modify :: (a -> a) -> Int -> [a] -> [a]
modify f 0 (x:xs) = f x : xs
modify f n (x:xs) = x : modify f (n-1) xs

initial :: [Int] -> [Int]
initial = foldr (modify succ) (replicate 9 0)

nextgen :: [Int] -> [Int]
nextgen (x:xs) = modify (+x) 6 xs ++ [x]

solution n = sum . last . take (n + 1) . iterate nextgen . initial

main :: IO ()
main = do
     input <- getContents
     case parse (sepBy natural (char ',')) input of
	 [] -> error "Input error"
	 [(l, "")] -> print $ solution 256 l
	 _ -> error "Input error"