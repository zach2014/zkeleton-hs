module ZBase (someFunc, zeroTo, joinArray, odds, isPrime, factors, primeFactors, quickSort, safeHead', knapsack) where
import Data.Array

-- | Some function that does nothing
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | safe head of list
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- | safe tail of list
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

head' :: [a] -> a
head' (x:_) = x;

safeHead' :: [a] -> Maybe a
safeHead' xs | null xs = Nothing 
             | otherwise = Just (head' xs)


-- | Generate a list of integers from 0 to n
zeroTo :: Int -> [Int]
zeroTo n  = [0..n]

-- | Generate a string from an array of integers
joinArray :: [Int] -> String
joinArray [] = ""
joinArray (x:xs) =  if null xs then show x else show x ++ ", " ++ joinArray xs


-- | odds of numbers from 0 to n 
odds :: Int -> [Int]
-- odds n = map f [0..n] where f x = x * 2 + 1
odds n = map (\x -> x * 2 + 1) [0..n]

-- | concat all the elements of the list of lists.
myConcat :: [[a]]-> [a]
myConcat xxs = [x |  xs <- xxs, x <- xs ]

-- | factors of a number (non-negative)
factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

-- | isPrime a number (non-negative)
isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

-- | primeFactors of a number (non-negative)
primeFactors :: Int -> [Int]
primeFactors n = [x | x <- [2..n], isPrime x]


-- | quick sort algorithm.
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) = quickSort lhs ++ p : quickSort rhs
    where
        lhs = filter (< p) xs
        rhs = filter (>= p) xs

-- | zip recursive
zipRecursive :: [a] -> [b] -> [(a,b)]
zipRecursive [] _ = []
zipRecursive _ [] = []
zipRecursive (x:xs) (y:ys) = (x,y):zipRecursive xs ys

knapsack :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]
