module Main where

import qualified ZBase (someFunc, zeroTo, joinArray)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  ZBase.someFunc
  putStrLn $ ZBase.joinArray $ ZBase.zeroTo 5
