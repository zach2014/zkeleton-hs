module HTMLGenerator() where 
import Data.Functor.Contravariant (Op)

html :: String  -> String
html content = "<!DOCTYPE html><html>" <> content <> "</html>"

head :: String -> String
head = el "head"

title :: String -> String
title = el "title"

body :: String -> String
body = el "body"

p_ :: String -> String
p_ = el "p"

div_ :: String -> String
div_ = el "div"

span_ :: String -> String
span_ = el "span"


h1_ :: String -> String
h1_ = el "h1"

el :: String -> (String -> String)
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- | can not applied to following
a_ :: String -> String
a_ s = "<a href=\"https://www.google.com\">" <> s <> "</a>"

img_ :: String -> String
img_ src = "<img src=\"" <> src <> "\"/>"

newtype Ele tag attrs content = Ele { getEle :: (tag, attrs, content) } deriving  (Show, Eq)

instance Functor (Ele tag attrs) where
    fmap f (Ele (tag, attrs, content)) = Ele (tag, attrs, f content)