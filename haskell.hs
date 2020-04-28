import DataFile 

wordToken s = reverse (helperWordToken 0 s [])

helperWordToken :: Int -> String -> [String] -> [String]
helperWordToken x s acc = if x >= (length s) then acc
					--else if (firstString (splitAt (x+1) s))==" " then helperWordToken 0(secondString (splitAt (x+1) s)) acc
					else if s !! x == ' ' then 
											if length (firstString (splitAt (x) s)) == 0 then  helperWordToken (0) (secondString (splitAt (x+1) s)) acc
											else helperWordToken (0) (secondString (splitAt (x+1) s)) ((firstString (splitAt (x) s)):acc)
					
					else if find (s !! x) punct then 
												if length (firstString (splitAt x s)) == 0 then helperWordToken (0) (secondString (splitAt (x+1) s)) ([s !! x]:acc)
											 	else helperWordToken (0) (secondString (splitAt (x+1) s)) ([s !! x]:(firstString (splitAt x s)):acc)
					
					else if x == ((length s)-1) then (firstString (splitAt (x+1) s)):acc
					
					else helperWordToken (x+1) s acc
					
firstString (s1,s2) = s1
secondString (s1,s2) = s2

find _ [] = False
find x (y:xs) | x==y = True
				| otherwise = find x xs

				
wordTokenList [] = []
wordTokenList list = loopWordTokenList list []

loopWordTokenList [] acc = acc
loopWordTokenList (x:list) acc = loopWordTokenList list ( acc++(wordToken x) )


uniqueBigrams list = bigramsHelper list []

bigramsHelper [] acc = acc
bigramsHelper (x:[]) acc = acc
bigramsHelper (x:y:list) acc = if find (x,y) acc then bigramsHelper (y:list) acc else bigramsHelper (y:list) (acc++[(x,y)])


uniqueTrigrams list = trigramsHelper list []
trigramsHelper [] acc = acc
trigramsHelper (x:[]) acc = acc
trigramsHelper (x:y:[]) acc = acc
trigramsHelper (x:y:z:list) acc = if find (x,y,z) acc then trigramsHelper (y:z:list) acc 
								else trigramsHelper (y:z:list) (acc++[(x,y,z)])

normalBigrams x = normalBigramsHelper x []
normalBigramsHelper [] acc = acc
normalBigramsHelper [x] acc =  acc
normalBigramsHelper (x:y:xs) acc = normalBigramsHelper (y:xs) (acc++[(x,y)])

bigramsFreq :: Num a => [String] -> [((String,String),a)]
bigramsFreq x = helperBigramsFreq (uniqueBigrams x) [] ( normalBigrams x)
helperBigramsFreq [] acc _ = acc 
helperBigramsFreq ((x,y):xs)  acc list =  if find2 (x,y) acc == False then  
								helperBigramsFreq xs (acc++[((x,y),(numberofocc1 (x,y) list))]) list
								else helperBigramsFreq xs acc list 
numberofocc1 _ [] = 0 
numberofocc1 (x1,y1) ((x2,y2):rest) | x1==x2 && y1==y2 = 1+numberofocc1 (x1,y1) rest
                                    | otherwise = numberofocc1 (x1,y1) rest
find2 _ [] = False 
find2 (x1,y1)(((x2,y2),z):xs)  |x1==x2 &&y1==y2 = True 
							 |otherwise = find2 (x1,y1) xs
							 

normalTrigrams x = normalTrigramsHelper x []
normalTrigramsHelper [] acc = acc 
normalTrigramsHelper [x] acc = acc 
normalTrigramsHelper [x,y] acc = acc 
normalTrigramsHelper (x:y:z:xs) acc = normalTrigramsHelper (y:z:xs) (acc++[(x,y,z)])
							 
trigramsFreq x = helperTrigramsFreq (uniqueTrigrams x)[] (normalTrigrams x)
helperTrigramsFreq [] acc _= acc 
helperTrigramsFreq ((x,y,z):xs) acc list = if find3 (x,y,z) acc == False then 
								helperTrigramsFreq xs (acc++[((x,y,z),(numberofocc2 (x,y,z) list))]) list
								else helperTrigramsFreq xs acc list 
numberofocc2 _ [] = 0 
numberofocc2 (x1,y1,z1) ((x2,y2,z2):rest) | x1==x2 && y1==y2 &&z1==z2= 1+numberofocc2 (x1,y1,z1) rest
                                    | otherwise = numberofocc2 (x1,y1,z1) rest
find3 _ [] = False 
find3 (x1,y1,z1)(((x2,y2,z2),z):xs)  |x1==x2 &&y1==y2&&z1==z2 = True 
							 |otherwise = find3 (x1,y1,z1) xs
							 
							 
getFreq str [] = 0
getFreq str ( (s,x):xs) = if str == s then x else getFreq str xs


generateOneProb _ [] = 0.0
generateOneProb ((x,y,z), a) ( ((x1,y1),b):xs) = if x == x1 && y == y1 then a/b else generateOneProb ((x,y,z), a) xs


genProbPairs :: Fractional a => [((String,String,String),a)] -> [((String,String),a)] -> [((String,String,String),a)]
genProbPairs triList biList = pairsHelper1 triList biList []

pairsHelper1 [] _ acc = acc
pairsHelper1 ( ((x,y,z),a):triList) biList acc = pairsHelper1 triList biList (acc++(pairsHelper2 ((x,y,z),a) biList))

pairsHelper2 ((x,y,z),a) [] = [((x,y,z),0)]
pairsHelper2 ((x,y,z),a) (((x1,y1),b):biList) = if x == x1 && y == y1 then [((x,y,z), (a/b))]
												else pairsHelper2 ((x,y,z),a) biList


generateNextWord _ [] = error "Sorry, it is not possible to infer from current database"
generateNextWord words list = nextWordHelper words (list !! (randomZeroToX ((length list) - 1))) list

nextWordHelper [x,y] ( (a,b,c),d) list = if x==a && y==b && d > 0.03 then c else generateNextWord [x,y] (remove ((a,b,c),d) list)

remove _ [] = []
remove x (y:ys) = if x == y then ys else y:(remove x ys)


generateText str 0 = str
generateText str n = generateText (str++" "++(generateNextWord (getLast2 (wordToken str)) (genProbPairs (trigramsFreq d) (bigramsFreq d)) )) (n-1)
					where d = wordTokenList docs

getLast2 (x:words) = if length (x:words) <= 2 then (x:words) else getLast2 words
