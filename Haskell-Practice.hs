{-
Define the function delete :: Int -> [Int] -> [Int] that deletes all occurrences of the first argument from the list given as a second argument.
-}
delete :: Int -> [Int] -> [Int]
delete x [] = []
delete x (y:ys) | x==y      = delete x ys    -- remove x and keep on deleting
                | otherwise = y:delete x ys  -- keep y and keep on deleting

{-
Define the function insert :: Int -> [Int] -> [Int] that inserts an integer at the correct position into a sorted list.
-}
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) | x<y       = x:y:ys         -- position found, insert x here
                | otherwise = y:insert x ys  -- insert x further up the list


{-
Define a function apply that takes a function f as an argument and a value x and returns the value of f for x. First, define the type of apply. Then give the definition.
-}
apply :: (a -> b) -> a -> b
apply f x = f x

{-
Define a function mapPair that takes a function f and a pair of values as arguments and returns the values of f for both components of the pair, i.e., the return result will also be a pair. First, define the type of the function, and then give the definition.
-}
mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x,f y)

{-
Define a function applyTwo that takes two functions f and g as arguments as well as a single value x and returns the values of f and g when applied to x.  The return result will be a pair. When you define the type of the function, note that return types of f and g do not have to be the same.

Also, give a definition for the value evenSuc2 :: (Bool,Int) that uses applyTwo to compute whether 2 is even as well as its successor.
-}
applyTwo :: (a -> b) -> (a -> c) -> a -> (b,c)
applyTwo f g x = (f x,g x)

evenSuc2 :: (Bool,Int)
evenSuc2 = applyTwo even succ 2

{-
Define the function applyPair that is almost identical to applyTwo but takes a pair of functions as arguments.
-}
applyPair :: (a -> b,a -> c) -> a -> (b,c)
applyPair (f,g) x = (f x,g x)
-- applyPair p x = applyTwo (fst p) (snd p) x

{-
Define a function parallel that takes a pair of functions and applies it to a pair of values. Make sure the type is as general as possible.
-}
parallel :: (a -> c,b -> d) -> (a,b) -> (c,d)
parallel (f,g) (x,y) = (f x,g y)
