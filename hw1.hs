
type Bag a = [(a,Int)]
curBag::Bag Int
curBag = [(1,1),(1,2),(3,2)]
curBag2::Bag Int
curBag2 = [(1,1),(2,2),(3,1)]

ins :: Eq a => a -> Bag a -> Bag a 
ins x [] = [(x,1)]--no match bag so insert new item
ins x ((y,n):ys)
        | x==y = (y,n+1):ys --find same item in bag
        | otherwise = (y,n):ins x ys --continue find bag
----------------------------------------------------------------
del :: Eq a => a -> Bag a -> Bag a

del x ((y,n):ys)
        | x==y && n>1 = (y,n-1):ys --in bag(more than 1)
        | x==y && n==1 = ys--remove the item in bag
        | otherwise = (y,n):del x ys --find
----------------------------------------------------------------
bag :: Eq a => [a] -> Bag a
bag = foldr ins [] 
-- bag xs = foldr ins [] xs   
-- bag [] = []
-- bag (x : xs) = in s x (bag xs)  
-- bag xs = map ins xs

----------------------------------------------------------------
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] b = True
subbag a [] = False

subbag (x:xs) ys
     | findInSub x ys = subbag xs ys --find current item in ys or not
     | otherwise = False

findInSub :: Eq a =>(a, Int)->Bag a->Bool --caution type
findInSub a [] = False
findInSub x ((y,n):ys) 
    | (fst x == y) && (snd x<=n) = True
    | (fst x == y) && (snd x>n) = False
    | otherwise = findInSub x ys

----------------------------------------------------------------
isSet :: Eq a => Bag a -> Bool

isSet [] = True
isSet (x:xs) = findNotInBag (fst x) xs

findNotInBag :: Eq a => a -> Bag a -> Bool
findNotInBag x [] = True
findNotInBag x (y:ys) 
    | x == fst y = False
    | otherwise = findNotInBag x ys

----------------------------------------------------------------
size :: Bag a -> Int -- not sure to return how many kind of item or how many item
--ver 1 -- return how many kind of item
--size = foldr (\ x -> (+) 1) 0
--ver 2 --how many item
size [] = 0
size ((x,n):xs) = n+size xs

---------------------------------
--Q2
--a 
applyAll :: [(a -> b)]->a->[b]
applyAll fs x = [f x | f <- fs]

--b
type Property a = [a -> Bool]
digit :: Property Int
digit = [(>=0),(<10)]
satisfies :: Property a -> a -> Bool
satisfies bs x = and (applyAll bs x)

--c
power :: (a -> a) -> Int -> a -> a
power f 0 y = y
power f x y = f (power f (x-1) y) --recursive to smaller times

--d
plus :: Int -> Int -> Int 
plus a b = power succ a b

times :: Int -> Int -> Int
times 0 b = 0
times a 0 = 0
times a b = plus a (times a (b-1))