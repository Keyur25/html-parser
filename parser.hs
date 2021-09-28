-- based on https://stackoverflow.com/questions/48369242/in-haskell-how-can-i-get-a-substring-of-a-text-between-two-indices 
substring :: String -> Int -> Int -> String -- exclusive of end 
substring string start end = take (end - start) (drop start string)

indexOfEndOfTag :: String -> Int -> Int
indexOfEndOfTag ('>':xs) num = num
indexOfEndOfTag (' ':xs) num = num
indexOfEndOfTag (x:xs) num = indexOfEndOfTag xs (num+1)

removeAttributesFrom :: String -> String -- removes all the attributes from a tag e.g. <div class="main"> ---> <div>
removeAttributesFrom token = (substring token 0 ((indexOfEndOfTag token 0))) ++ ">"

lexicalAnalysis :: String -> String  -> [String] -> Bool -> [String] -- tokenises the input
lexicalAnalysis [] tag arr tagFound =  reverse arr -- return the reverse of arr as when we add the tags we are adding it to the start
lexicalAnalysis (x:xs) tag arr tagFound | ((x=='<') && (tagFound==True)) = []
                                        | (x=='<') = lexicalAnalysis xs "<" arr True
                                        | (x=='>') = (lexicalAnalysis xs "" ([removeAttributesFrom (tag++[x])] ++ arr) False)
                                        | (tagFound==True) = (lexicalAnalysis xs (tag++[x]) arr True)
                                        | otherwise = lexicalAnalysis xs tag arr tagFound

isAllTokensNestedWithHTML :: [String] -> Bool -- checks if all the tokens are nested by the html tags and that no html tags appear elsewhere
isAllTokensNestedWithHTML allTokens = ((allTokens !! 0)=="<html>") && ((allTokens !! lastIndex)=="</html>") && (not (("<html>" `elem` allTokensExludingHTMLTags))) && (not (("</html>" `elem` allTokensExludingHTMLTags)))
                                    where lastIndex = length(allTokens)-1
                                          allTokensExludingHTMLTags = take (length(allTokens)-2) (tail(allTokens))


verifyAllTokens :: [String] -> Bool -- checks if all the tokens are valid tags
verifyAllTokens [] = True
verifyAllTokens (x:xs) = if (x `elem` validTokens)==True then verifyAllTokens xs else False
                       where validTokens = ["<html>", "</html>", "<head>", "</head>", "<body>", "</body>", "<title>", "</title>", "<h1>", "</h1>", "<h2>", "</h2>", "<h3>", "</h3>", "<p>", "</p>","<ul>", "</ul>", "<li>", "</li>", "<a>", "</a>", "<div>", "</div>", "<br>", "<hr>"] 

isPInStack :: [String] -> Bool -- checks if <p> is in the stack
isPInStack arr = or ([x=="<p>" | x <- arr])

startTagOf :: String -> String -- given a valid close tag, this function gives the start tag of it e.g. </title> ---> <title>
startTagOf tag = "<" ++ (substring tag 2 (length(tag)))

isTokensNestedCorrectly :: [String] -> [String] -> Int -> Bool
isTokensNestedCorrectly [] [] 4 = True
isTokensNestedCorrectly [] stack checkStructure = False
isTokensNestedCorrectly (x:xs) stack checkStructure | ((x=="<br>") || (x=="<hr>")) = isTokensNestedCorrectly xs stack checkStructure
                                                    | ((x=="<head>") && (checkStructure/=0)) = False
                                                    | ((x=="<head>") && (checkStructure==0)) = isTokensNestedCorrectly xs (x:stack) (checkStructure+1)
                                                    | ((x=="<title>") && (checkStructure/=1)) = False
                                                    | ((x=="</title>") && ((checkStructure/=1) || (head stack)/=("<title>"))) = False
                                                    | ((x=="</title>") && ((checkStructure==1) && (head stack)==("<title>"))) =  isTokensNestedCorrectly xs (tail(stack)) checkStructure
                                                    | ((x=="</head>") && ((checkStructure/=1) || (head stack)/=("<head>"))) = False
                                                    | ((x=="</head>") && ((checkStructure==1) && (head stack)==("<head>"))) = isTokensNestedCorrectly xs (tail(stack)) (checkStructure+1)
                                                    | ((x=="<body>") && (checkStructure/=2)) = False
                                                    | ((x=="<body>") && (checkStructure==2)) = isTokensNestedCorrectly xs (x:stack) (checkStructure+1)
                                                    | ((x=="</body>") && ((checkStructure/=3) || (head stack)/=("<body>"))) = False
                                                    | ((x=="</body>") && ((checkStructure==3) && (head stack)==("<body>"))) = isTokensNestedCorrectly xs (tail(stack)) (checkStructure+1)
                                                    | (((x=="<p>") || (x=="<div>")) && ((isPInStack stack)==True)) = False
                                                    | (((substring x 0 2)=="</") && ((head stack)/=(y))) = False
                                                    | (((substring x 0 2)=="</") && ((head stack)==(y))) = isTokensNestedCorrectly xs (tail(stack)) checkStructure
                                                    | otherwise = isTokensNestedCorrectly xs (x:stack) checkStructure
                                                    where y = startTagOf x

main = do
    allChars <- Prelude.readFile "file.html" -- https://stackoverflow.com/questions/7867723/haskell-file-reading/7867786
    let allTokens = lexicalAnalysis allChars "" [] False
    if ((length allTokens)==0) then putStrLn "INVALID HTML!" 
    else if ((isAllTokensNestedWithHTML allTokens)==False) then putStrLn "FILE NOT WRAPPED AROUND <html> </html>!" 
    else if ((verifyAllTokens allTokens)==False) then putStrLn "INVALID HTML TAG FOUND!" 
    else if ((isTokensNestedCorrectly allTokens [] 0)==False) then putStrLn "TAGS NOT NESTED CORRECTLY!" 
    else putStrLn "VALID HTML!"   