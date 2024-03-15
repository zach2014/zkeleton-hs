{-# LANGUAGE OverloadedStrings  #-}
import Test.HUnit (assertEqual, assertBool, assertString, assertFailure)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck (forAll, choose, Property, Arbitrary, Testable, arbitrary, Gen)
import Data.Maybe (isNothing)
import Data.Aeson
import ZBase
import ZData
import ZHTML
import ZStructure

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "group1"
    [  testCase "case1" $ assertBool "true is true" True],
    testGroup "group2" 
    [  testCase "case1" $ assertBool "numbers is prime" $ isPrime 257,
       testCase "case2" $ assertEqual "join ints"  "1, 2" (joinArray [1,2]),
       testCase "case3" $ assertEqual  "zero to five" [0..5] (zeroTo 5),
       testCase "case4" $ assertEqual "odds of five" [1,3,5,7,9,11] (odds 5),
       testCase "case4" $ assertEqual "quick sort" [1,3,5,7,9,11] (quickSort [1,5,7,9,3,11]),
       testCase "case5" $ assertBool "safe head" (isNothing $ safeHead' [])
    ],
    testGroup "groceries" 
    [
       testCase "check need pay" $ assertBool "need pay for King" (needPay king),
       testCase "check need pay" $ assertBool "need pay for Shoe" (needPay shoe),
       testCase "check show" $ assertEqual "can be show" "Shoe"  (show shoe),
       testCase "check knapsack" $ assertEqual "same consequence" 10 (knapsack [3, 4, 5, 8 , 10] [2, 3 ,4, 5, 9] 20) 
    ], 
    testGroup "safe division" [
        testCase "failure"  $ assertEqual  "division by zero fails" failure (safeDivide 10 0),
        testCase "success"  $ assertEqual "division is correct" (Just 5) ( extractValue (safeDivide 10 2)),
        testCase "failure"  $ assertEqual  "division by zero fails" Nothing (extractValue $ safeDivide 10 0)
    ],
    testGroup "tree traversal" [
        testCase "constr root"  $ assertEqual  "root is empty"  "" (printTree root),
        testCase "constr leaf"  $ assertEqual  "leaf is a"  "a" (printTree (leaf 'a')),
        testCase "constr node"  $ assertEqual  "node is bac"  "bac" (printTree (node (leaf 'b') 'a' (leaf 'c'))),
        testCase "node associated 1"  $ assertEqual  "3 leafs associated"  "abc" (printTree (leaf 'a' <> leaf 'b' <> leaf 'c')),
        testCase "node associated 2"  $ assertEqual  "4 leafs associated"  "abcd" (printTree (leaf 'a' <> leaf 'b' <> leaf 'c' <>  leaf 'd')),
        testCase "node associated 3"  $ assertEqual  "associated with root one"  "abc" (printTree (leaf 'a' <> leaf 'b' <> leaf 'c'<> root))
    ],
    testGroup "record syntax" [
        testCase "show knight"  $ assertEqual  "not empty string"  "" (show Knight { name = "Fred", quest = "nothing", age = 25, favoriteWeapon = "Sword"}),
        testCase "to json knight"  $ assertEqual  "not empty json string"  "" (encode Knight { name = "ToJson", quest = "json quest", age = 25, favoriteWeapon = "Sword"})
    ],
    testGroup "data structure" [
        testCase "empty tree"  $ assertEqual  "empty list to empty tree"  Empty  (treeFromList [] :: BinTree Int),
        testCase "limited tree"  $ assertEqual  "123456 in tree"  (treeFromList' [1..5] :: BinTree Int) (treeFromList [1..5]::BinTree Int)
    ]
    ]