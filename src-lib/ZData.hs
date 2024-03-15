{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# LANGUAGE DeriveGeneric #-}

module ZData(Grocery, shoe, king, groceries, needPay, NumberDivisionResult, failure, safeDivide, extractValue, Tree(..), root, leaf, node, printTree, Knight(..)) where
import GHC.Natural (Natural)
import GHC.Generics
import Data.Aeson

data Grocery = Shoe | Ship | Cabbage | King deriving (Show, Eq)

shoe :: Grocery
shoe = Shoe

king :: Grocery
king = King

groceries :: [Grocery]
groceries = [Shoe, Ship, Cabbage, King ]

needPay :: Grocery -> Bool
needPay Shoe  = True
needPay Ship = True
needPay Cabbage = False
needPay King = True


data  NumberDivisionResult = Failure  | Ok Natural
  deriving (Show, Eq)

failure :: NumberDivisionResult
failure = Failure

extractValue :: NumberDivisionResult -> Maybe Natural
extractValue (Ok n) = Just n
extractValue Failure = Nothing

extractValue' :: NumberDivisionResult -> Maybe Natural
extractValue'  res = case res of
    Failure -> Nothing
    Ok x   -> Just x

safeDivide :: Natural -> Natural -> NumberDivisionResult
safeDivide _ 0 = Failure
safeDivide x y = Ok $ x `div` y


data GroceryShop  = GroceryShop String Natural [Grocery]
  deriving Show

instance Eq GroceryShop where
    (==) (GroceryShop name1 value1 _) (GroceryShop name2 value2 _) = name1 == name2 && value1 == value2

getName :: GroceryShop -> String
getName (GroceryShop name _ _) = name

getValue :: GroceryShop -> Natural
getValue (GroceryShop _ value _) = value

getGroceryList :: GroceryShop -> [Grocery]
getGroceryList (GroceryShop _ _ stock) = stock

addItem :: Grocery -> GroceryShop -> GroceryShop
addItem grocery (GroceryShop name value stock) = GroceryShop name value (grocery : stock)

removeItem :: Grocery -> GroceryShop -> GroceryShop
removeItem  grocery (GroceryShop name value stock) = GroceryShop name value (filter (/= grocery) stock)


data Tree = Root | Leaf Char | Node Tree Char Tree deriving Show

root :: Tree
root = Root

leaf :: Char -> Tree
leaf = Leaf

node :: Tree -> Char -> Tree -> Tree
node = Node


instance Semigroup Tree where
    (<>) Root Root = Root
    (<>) Root x = x
    (<>) x Root = x
    (<>) (Leaf l) (Leaf r) = Node Root l (Leaf r)
    (<>) (Leaf l) x = Node Root l x
    (<>) (Node l a b) (Leaf r) = Node  l a (b <> Leaf r)
    (<>) (Node l1 x1 r1) y = Node l1 x1 (r1 <> y)

instance Monoid Tree where
    mempty = Root
    mappend Root x = x
    mappend x Root = x
    mappend x y = x <> y

countLeaf :: Tree -> Int
countLeaf Root = 0
countLeaf (Leaf _) = 1
countLeaf (Node l _ r) = countLeaf l + countLeaf r

countNode :: Tree -> Int
countNode Root = 0
countNode (Leaf _) = 0
countNode (Node l _ r) = 1 + countNode l + countNode r

countCharOf :: Char -> Tree -> Int
countCharOf c Root = 0
countCharOf  c (Leaf x) = if x == c then 1 else 0
countCharOf c (Node l x r) = if x == c then 1 + countCharOf c l + countCharOf c r else countCharOf c l + countCharOf c r

printTree :: Tree -> String
printTree Root = []
printTree (Leaf x) = [x]
printTree (Node l x r) = printTree l ++ [x] ++ printTree r


data Tree' a = Leaf' | Node' a (Tree' a) (Tree' a) deriving Show

countLeaf'  :: Tree' a -> Int
countLeaf' Leaf' = 1
countLeaf' (Node' _ l r) = countLeaf' l + countLeaf' r

countNode' :: Tree' a -> Int

countNode' Leaf' = 0
countNode' (Node' _ l r) = 1 + countNode' l + countNode' r


-- | type is alias for existed type constructor
-- | Description and Theme are completely same one type String for compiler

type Description = String

type Theme = String

showDescWithTheme :: Description -> Theme -> String
showDescWithTheme desc theme = "<div class=\"" <> theme ++ "\">" <> desc <> "</div>"


-- | data with your specified type constructor
-- | Description' and Theme' are completely different type for compiler

data Description' = ShortDescription String | LongDescription String deriving Show

data Theme' = OldFashing String | NewFashion String

showDescWithTheme' :: Description' -> Theme' -> String
showDescWithTheme' desc theme = case desc of
    (ShortDescription s) -> "<div class=\"" <> getTheme' theme <> "\">" <> s <> "</div>"
    (LongDescription l) -> "<div class=\"" <> getTheme' theme <> "\">" <> l <> "</div>"

getDescription' :: Description' -> String 
getDescription' (ShortDescription s) = s
getDescription' (LongDescription l) = l

getTheme' :: Theme' -> String
getTheme' (OldFashing t) = t
getTheme' (NewFashion t) = t

data Knight = {-hi-} Knight {name :: String , quest :: String,  favoriteWeapon :: String, age :: Int} deriving (Show, Generic){-/hi-}

instance ToJSON Knight where
    toEncoding = genericToEncoding defaultOptions

