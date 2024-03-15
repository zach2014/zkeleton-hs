module ZHTML (makeHtml, makeDiv, makeButton, makeTable) where 

-- | make html 

makeHtml :: String -> String -> String
makeHtml title content = "<!DOCTYPE html><html><head><title>" ++ title ++ "</title></head><body>" ++ content ++ "</body></html>"


-- | make a div with an id and some content
makeDiv :: String -> String -> String
makeDiv id content = "<div id=\"" ++ id ++ "\">" ++ content ++ "</div>"

-- | make a button with an id, label and onclick function
makeButton :: String -> String -> String -> String
makeButton id label onclick = "<button id=\"" ++ id ++ "\" onclick=\"" ++ onclick ++ "\">" ++ label ++ "</button>"

-- | make a div with an id and some content
makeTable :: String -> [String] -> String
makeTable id rows = "<table id=\"" ++ id ++ "\"><tr>" ++ concatMap (\row -> "<td>" ++ row ++ "</td>") rows ++ "</tr></table>"

-- | make a div with an id and some content
makeDivWithContent :: String -> String -> String
makeDivWithContent id content = "<div id=\"" ++ id ++ "\">" ++ content ++ "</div>"