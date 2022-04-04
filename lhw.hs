



mxmInt :: [Int] -> Int
mxmInt [] = error "empty list" 
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

removeFst ::(Eq a) => a -> [a] -> [a]
removeFst = deleteBy (==)

--look below for details on deleteBy

lastButOne :: [a] -> a
lastButOne xs = last (init(xs))

divides d n = rem n d ==0

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

pygmy x (y:ys) = length (filter (==x) ys)
--EUREKA pygmy worked to find the amount of an element in a list
-- exercise 1.13 p 27

-- 1.14 answer is curly


prsk (x:xs) = x : replicate ((length (x:xs))-1) x ++ (prsk (xs))
prsk [] = []

priik (x:xs) = reverse (x : replicate ((length (x:xs))-1) x ++ (prsk (xs)))
priik [] = []

piiy (y:ys) = reverse (y:ys)

curly (y:ys) = priik (piiy(y:ys))

--EUREKA p 27 1.14 curly which contains piiy and priik and prsk 
--fulfills the function of bang! lOOK ABOVE

sigk (x:xs) = map length (x:xs) --will pass
-- EUREKA  answer to 1.2

lek (x:xs) = sum ( sigk (x:xs))
-- answer to 1.21

srk xs = m : (srk (removeFst m xs)) where m = minimum xs



prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

--kiffer:: String -> String -> Bool
--kiffer:: [] ys = True
--kiffer:: (x:xs) [] = False
--kiffer:: (x:xs) (y:ys) = 

--ys = prefix xs
--let xs = y in (y:ys)
-- xs = y where (y:ys)
--if y == y:ys && xs 


--pit :: String -> String -> [Char]
--pit xs ys = xs >= ys = "not a substring"
--pit xs ys = xs == ys = "same thing"

kk (y:ys) = tail (y:ys)

lock (x:xs) (y:ys) = if prefix (x:xs) (y:ys) == False 
                     then prefix (x:xs) (kk (y:ys)) 
                     else True

koft (x:xs) (y:ys) = if lock (x:xs) (y:ys) == False
                     then lock (x:xs) (kk (ys))
                     else True
              
koft [] (y:ys) = True
koft (x:xs) [] = False


-- for substring import Data.List and use
-- "isInfixOf"







lemon :: [xs] -> [Int]
lemon (x:xs) = map length [(x:xs)]
-- What's wrong with this

opp (x:xs) = length (x) : opp xs

fict (x:xs) = length x : fict xs


sike [xs] = length [xs]

tit xs = length xs -- will succeed
--answer to 1.21

pit [(x:xs)] = length [(x:xs)]


tike [[a]] = map length [[a]]
tike [] = []
tike [[]] = []
--What's wrong wit this?

sick xs = map length xs -- will pass



sifk (x:xs) = map length (x:xs) --will pass














kiffer (x:xs) (y:ys) = x==y || if x/=y then koft (x:xs) (kk (y:ys)) else True


--stuv (x:xs) (y:ys) = prefix (x:xs) (y:ys)
--stuv (x:xs) (y:ys) = if prefix (x:xs) (y:ys) == False then prefix (x:xs) (ys) else stuv (x:xs) (y:ys)

--pleak :: String -> String -> Bool
--pleak (x:xs) (y:ys) = (filter (==(x:xs)) (y:ys)) == (x:xs)
--pleak (x:xs) (y:ys) = pleak (x:xs) (tail (y:ys) )

--LOOK AT 
--			any
--			elem



subu :: String -> String -> Bool
subu [] ys = True
subu (x:xs) [] = False
subu (x:xs) (y:ys) = xs ==ys || x == y

plack (x:xs) (y:ys) = x==y || xs == ys 


scorm (x:xs) (y:ys) = subsequences (x:xs) == subsequences (y:ys)


pleck :: String -> String -> Bool
pleck (f:fs) (d:ds) = f : ds == d:fs

pluck :: String -> String -> Bool
pluck (f:fs) (d:ds) = [f]==ds


vluk (x:xs) (y:ys) = plack xs : ys



subsequences            :: [a] -> [[a]]
subsequences xs         =  [] : nonEmptySubsequences xs

-- | The 'nonEmptySubsequences' function returns the list of all subsequences of the argument,
--   except for the empty list.
--
-- >>> nonEmptySubsequences "abc"
-- ["a","b","ab","c","ac","bc","abc"]
nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r 



subd :: String -> String -> Bool
subd [] ys = True
subd (x:xs) [] = False
subd (x:xs) (y:ys) = (x==y) || subd xs ys && length xs == length ys
--subd (x:xs) (y:ys) =

subp :: String -> String -> Bool
subp [] ys = True
subp (x:xs) [] = False
subp (x:xs) (y:ys) = xs ==ys



subq :: String -> String -> Bool
subq [] ys = True
subq (x:xs) [] = False
subq (x:xs) (y:ys) = xs ==ys




keft (x:xs) (y:ys) = x ==y 
skeft xs ys = keft xs ys

killd (x:xs) (y:ys) = elem x y

kd (x:xs) (y:ys) = if x == y then delete y (y:ys) else xs 

kf (x:xs) (y:ys) = if x /= y then delete y (y:ys) else  ys


ffick (x:xs) (y:ys) = x : kd xs ys


ink x xs = map (delete x) (xs)




--mytake x xs = take x xs

-- must have something to do with take and nub
-- use https://wiki.haskell.org/How_to_work_on_lists
-- use https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html
-- use https://hackage.haskell.org/package/base-4.15.0.0/docs/src/Data-OldList.html#delete
-- sublist, delete


--from source, hackage

delete                  :: (Eq a) => a -> [a] -> [a]
delete                  =  deleteBy (==)

-- | \(\mathcal{O}(n)\). The 'deleteBy' function behaves like 'delete', but
-- takes a user-supplied equality predicate.
--
-- >>> deleteBy (<=) 4 [1..10]
-- [1,2,3,5,6,7,8,9,10]
deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _  _ []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys

-- | The '\\' function is list difference (non-associative).
-- In the result of @xs@ '\\' @ys@, the first occurrence of each element of
-- @ys@ in turn (if any) has been removed from @xs@.  Thus
--
-- > (xs ++ ys) \\ xs == ys.
--
-- >>> "Hello World!" \\ "ell W"
-- "Hoorld!"
--
-- It is a special case of 'deleteFirstsBy', which allows the programmer
-- to supply their own equality test.

(\\)                    :: (Eq a) => [a] -> [a] -> [a]
(\\)                    =  foldl (flip delete)

-- | The 'union' function returns the list union of the two lists.
-- For example,
--
-- >>> "dog" `union` "cow"
-- "dogcw"
--
-- Duplicates, and elements of the first list, are removed from the
-- the second list, but if the first list contains duplicates, so will
-- the result.
-- It is a special case of 'unionBy', which allows the programmer to supply
-- their own equality test.




-- look at elem
-- look at delete
-- look at filter
-- look at https://www2.math.ou.edu/~dmccullough/teaching/f06-6833/haskell/map_filter.pdf



--look at deleteby for a clue us eq
-- deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys
-- look at null

--look at other delete
--delete :: Eq a => a -> [a] -> [a]
--delete x [] = []
--delete x (y:ys) | x == y = ys
--				  | otherwise = y : delete x ys 




-- | \(\mathcal{O}(n)\). 'filter', applied to a predicate and a list, returns
-- the list of those elements that satisfy the predicate; i.e.,
--
-- > filter p xs = [ x | x <- xs, p x]
--
-- >>> filter odd [1, 2, 3]
-- [1,3]
--{-# NOINLINE [1] filter #-}
--filter :: (a -> Bool) -> [a] -> [a]
--filter _pred []    = []
--filter pred (x:xs)
--  | pred x         = x : filter pred xs
--  | otherwise      = filter pred xs

--{-# INLINE [0] filterFB #-} -- See Note [Inline FB functions]
--filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
--filterFB c p x r | p x       = x `c` r
--                 | otherwise = r

--{-# RULES
--"filter"     [~1] forall p xs.  filter p xs = build (\c n -> foldr (filterFB c p) n xs)
--"filterList" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
--"filterFB"        forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
-- #-}

-- Note the filterFB rule, which has p and q the "wrong way round" in the RHS.
--     filterFB (filterFB c p) q a b
--   = if q a then filterFB c p a b else b
--   = if q a then (if p a then c a b else b) else b
--   = if q a && p a then c a b else b
--   = filterFB c (\x -> q x && p x) a b
-- I originally wrote (\x -> p x && q x), which is wrong, and actually
-- gave rise to a live bug report.  SLPJ.



--elem :: Eq a => a -> t a -> Bool
--    elem = any . (==)


-- Exercises in Lists

--peck 6 = 15
--peck 2x = 44
--peck even x = 99
--peck odd x = 44
--peck x = x + peck x where x <33

-- infinite, !

unpeck x = x - unpeck x 
-- infinite, !



frog :: Integer -> Bool
frog x = if rem x 4 == 0 || (rem x 400 == 0 && rem x 100 == 0)  then True else False
 
fritch x = rem x 400 == 0 && rem x 100 == 0 

zift x = rem x 4 == 0 && rem x 8 ==0

zlick :: Integer -> Bool
zlick x = (rem x 4 == 0 && rem x 100 /= 0) || rem x 400 == 0
-- EUREKA zlick can check if any year is a Gregorian Leap year

zick year = (rem year 4 == 0 && rem year 100 /= 0) || rem year 400 == 0



-- 16 * 25 | 2^4 * 5^2 | 
-- 3 * 2^2 | 2^2 * 5   | 
-- (4 * 10^x) * x

add t y = [t / (1+y) | y <- [1..y]]
     
-- how to limit the range of a list?

double :: Integer -> Integer
double x = 2 * x

