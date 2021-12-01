import Control.Monad (liftM)

numbers :: String -> [Int]
numbers = map read . lines

solution1 :: [Int] -> Int
solution1 ns = sum $ map (\(a, b) -> if b - a > 0 then 1 else 0) $ zip ns (tail ns)

solution2 :: [Int] -> Int
solution2 ns = solution1 $ zipWith3 (\a b c -> a + b + c) ns (tail ns) (tail (tail ns))

main :: IO ()
main = do
    ns <- liftM numbers getContents
    print $ solution1 ns
    print $ solution2 ns