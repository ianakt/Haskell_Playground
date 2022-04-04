lastButOne :: [a] -> a
lastButOne (x:xs) = last (init [x])


lbO :: [a] -> a
lbO (x:xs) = last (init [x])

-- Why didn't the functions lbO and lastButOne not work but lz xs did?
-- brackets for "init [x]" and/or use of "x:xs" instead of "xs"

lz :: [a] -> a
lz [] = error "You can't do that"
lz xs = last (init(xs)) --Halleluajh! it worked!!!! :)
--lz [1..] = error "impossible, aka infinity" How do you represent an infinite list?




--sD :: Int -> [a] -> [a
--sD [x] [x] = drop length[x]

-- list of size zero or one give an error message
-- list of size 2 or greater  give a message of non exhaustive patterns
--Ctrl D copies the line
-- list of any size returns empty list

--(xs:x:x) !!!
--(x:xs)


-- last xs
-- (xs:x) (_:xs) what to do about these?

-- lbO (x:xs)    =  x : init xs


