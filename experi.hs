lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- But why doesn't this work when you copy/pase this in ghci?

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

--ORDER MATTERS in Pattern Matching

-- it's always best to specify the most specific ones first and then the more general ones later.

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

--A string is a list of characters

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName _ = "Not Albert not Broseph not Cecil"

-- When using "_" (the underscore) in patternmatching the input must match the type
--specified for the function
-- tuples are in () parenthesis, different types can be put in a tuple of any size
-- list are in square brackets[] only elements of the same type can be put in a list

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--What would you add to the function capital to ignore a space at the start of a string?

-- file: ch02/myDrop.hs
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

myDropX n xs = if n <= 0 || null xs 
               then xs 
			   else myDropX (n - 1) (tail xs)

isOdd n = mod n 2 == 1

-- For patternmatching the style is (input type) => input -> output	



-- I CAN NOT CREATE AN EXAMPLE OF PATTERN MATCHING

