module ChapterFour where

minimum' :: Ord t => [t] -> t
minimum' [a] = a
minimum' (a:rest) = if a < b then a else b
    where 
        b = (minimum' rest)

calcSum :: Num t => [t] -> t
calcSum [] = 0
calcSum (a:rest) = a + (calcSum rest)

count :: Num t => [a] -> t
count [] = 0
count (a:rest) = 1 + (count rest)

makeRange :: (Eq t, Num t) => t -> t -> [t]
makeRange a b 
    | a == b    = [a]
    | otherwise = [a] ++ (makeRange (a+1) b)

makeReverseRange :: (Eq t, Num t) => t -> t -> [t]
makeReverseRange a b
    | a == b    = [b]
    | otherwise = [b] ++ (makeReverseRange a (b-1))

notInList :: Eq t => t -> [t] -> Bool
notInList _ [] = True
notInList elem (a:rest)
    | elem == a = False
    | otherwise = notInList elem rest

square :: Num t => t -> t
square no = no^2

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (a:rest) = [square a] ++ (squareAll rest)

squareIfEven :: Integral t => [t] -> [t]
squareIfEven [] = []
squareIfEven (a:rest) = (if even a then [square a] else [a]) ++ (squareIfEven rest)

squareOnlyEven :: Integral t => [t] -> [t]
squareOnlyEven [] = []
squareOnlyEven (a:rest) = (if even a then [square a] else []) ++ (squareOnlyEven rest)

mergeSort :: Ord t => [t] -> [t] -> [t]
mergeSort a [] = a
mergeSort [] a = a
mergeSort listA@(a:restA) listB@(b:restB) = (if a < b then ([a] ++ mergeSort restA listB) else ([b] ++ mergeSort listA restB))

subList :: (Eq t, Num t) => t -> t -> [a] -> [a]
subList _ _ [] = []
subList _ 0 _  = []
subList 0 b (s:rest) = [s] ++ (subList 0 (b-1) rest)
subList a b (_:rest) = subList (a-1) b rest


