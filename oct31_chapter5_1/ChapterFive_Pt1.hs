module ChapterFive_Pt1 where

convert :: Floating t => t -> t -> t
convert input factor = input * factor

doMetersToFeet :: Floating t => t -> t
doMetersToFeet = convert 3.28084

doMilesToKM :: Floating t => t -> t
doMilesToKM = convert 1.60934

calcSalesTax :: Floating t => t -> t -> t
calcSalesTax = convert

doGolden :: Floating t => t -> t
doGolden = calcSalesTax 0.03

doBoulder :: Floating t => t -> t
doBoulder = calcSalesTax 0.0341

swap :: (t,t) -> (t,t)
swap (a,b) = (b,a)

swapAll :: [(t,t)] -> [(t,t)]
swapAll list = map swap list

applyIfTrue :: Num b => (a -> b) -> a -> Bool -> b
applyIfTrue _ _ False = 0
applyIfTrue f x True  = f x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

calcAreas :: Num t => [t] -> [t] -> [t]
calcAreas = zipWith (*)