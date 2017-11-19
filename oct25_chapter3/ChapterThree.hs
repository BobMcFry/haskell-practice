-- author: Christian Heiden (cheiden@mymail.mines.edu)

module ChapterThree where

    inList :: Eq t => t -> [t] ->[t]
    inList target list = [ x | x <- list, x == target]    

    square :: (Integral i, Floating f) => i -> f
    square no = (fromIntegral no) **2

    squareEvenNumbers :: (Integral i, Floating f) => [i] -> [f]
    squareEvenNumbers list = [ square x | x <- list, even x ]

    courseMajor :: String -> String
    courseMajor major@(i1:i2:_) = major ++ " is a " ++ initials ++ " course"
        where 
            initials = [i1] ++ [i2]

    threshold :: (Ord t, Num t) => t -> t -> t -> [Char]
    threshold price qty range
        | total < low = "Total is low"
        | total < medium = "Total is medium"
        | total < high = "Total is high"
        | total >= high = "Total is extraordinary"
        | otherwise = error "Invalid input."
        where
            total = price * qty
            low = range
            medium = range * 2
            high = range * 3

    lactate :: (Ord t, Fractional t) => t -> t -> [Char]
    lactate hr hr_max
        | frac < aerobic = "warmup"
        | frac < steadyState = "aerobic"
        | frac < anaerobic = "steadyState"
        | frac < competitive = "anaerobic"
        | otherwise = "wow, don't do this for long!"
        where
            frac = hr / hr_max
            easy = 0.6
            aerobic = 0.7
            steadyState = 0.8
            anaerobic = 0.9
            competitive = 1

    calcAreas :: Num t => [(t,t)] -> [t]
    calcAreas list = [ area tup | tup <- list ]
        where 
            area (l,w) = w*l

    calcTriangleAreas :: Fractional t => [(t,t)] -> [t]
    calcTriangleAreas list = [ area tup | tup <- list ]
        where 
            area (b,h) = (b*h)/2


    orderTwo :: Ord t => [t] -> [t]
    orderTwo ([]) = error "List must have exactly two elements"
    orderTwo (a:[]) = error "List must have exactly two elements"
    orderTwo (a:b:[])
        | a > b = [b,a]
        | otherwise = [a,b]
    orderTwo list = error "List must have exactly two elements"

    orderThree :: Ord t => [t] -> [t]
    orderThree (a:b:c:[])
        | c > last s = s ++ [c]
        | c < head s = [c] ++ s
        | otherwise = [head s, c, last s]
        where
            s = orderTwo [a,b]


-- Pattern Matching Pros:
    -- You want to decompose your input easily.
    -- Don't need evaluation of input inside of function body which makes code more readable/understandable.
-- Guards Pros:
    -- Makes writing "ifs" easier as guards are basically syntactic sugar for a series of if constructs.
    -- useful for working with several ranges like in a switch-case construct. 
