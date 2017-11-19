module ChapterFive_Pt3 where

getUserName :: [Char] -> [Char]
getUserName mail = takeWhile (/='@') mail

numAs :: (Num t, Ord t) => [t] -> Int
numAs scores = length $ filter (\a -> a >= 90) scores

totalDiscount :: Num t => t -> [t] -> t
totalDiscount percent list = foldl (+) 0 $ map (*percent) list

totalWithDiscount :: Num t => t -> [t] -> t
totalWithDiscount percent list = (foldl (+) 0 list) - (totalDiscount percent list)

discountedItems :: Num t => t -> [t] -> [t]
discountedItems percent list = foldr (:) [] $ zipWith (-) list $ map (*percent) list

anyBigNumbers :: (Ord a, Num a) => a -> [a] -> Bool
anyBigNumbers threshold list = foldr (\x acc -> if x > threshold then True else acc) False list

multTableRow :: Num t => t -> [t]
multTableRow a = map ($ a) [(1*), (2*), (3*), (4*), (5*), (6*), (7*), (8*), (9*), (10*)]



f1 x = 2*x + 3
f2 x = x*x
f3 x = 0.5*x - 1.5

testInverses :: (Num a, Eq a) => (a -> a) -> (a -> a) -> Bool
testInverses f g = oneSide == otherSide
    where
        vals = [-1,3,5,7,10]
        oneSide = map (f . g) vals
        otherSide = map (g . f) vals








