doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
-- How to do List Comprehension?
-- [function | range (set), condition(filter)]

-- in defining a command there are types x xs st and others
upOne x = x+1

divAug x =(x/(x+1))

triplemN :: Integer -> Integer
triplemN x = if x < 55 then 3*x else 0000



peb x = take x (cycle [1,2,3])

func x = if x <= 10 then 2*x else x

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- Why did this not work? The code below when executed shows a long list
--lucky x = [if x == 7 then "Lucky Number 7" else "Sorry, you'r out of luck, pal" | x <- [1,2.500]]
-- the range can get complicated with list comprehensions
--How did the range affect the output?
--This one did work though! lucky x = [if x == 7 then "Lucky Number 7" else "Sorry, you'r out of luck, pal"]

--Use:

lucky x = [if x == 7 then "Lucky Number 7" else "Sorry, you'r out of luck, pal"]


sayMe' x = [if x < 6 then "Yes" else "No", if x > 0 then "Yes" else "No"]
sayMe x = [if x < 6 && x > 0 then "Yes" else "No"]

-- I DON'T GET SYNTAX IN FUNCTIONS (-DON'T)
--PLEASE READ use experi
-- Create a new file and copy/paste commands into it, then load in ghci !!!!!
--types must be defined in the text and/or hs file before being compiled to work

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                 = "You're a whale, congratulations!"


-- How to manipulate functions further? use the operator and in haskell which is &&

-- A floating point # is one that is a member of Q (Quotient #)
-- functions have types, everything has a type

