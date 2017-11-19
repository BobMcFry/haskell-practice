import Control.Monad  

greeting :: IO ()
greeting = do
    putStrLn "Enter your name as last, first"
    name <- getLine
    let names = words (filter (/=',') name)
    putStrLn ("Hello " ++ (last names))
    putStrLn ("Do you have any relatives named Joe " ++ (head names) ++ "?")

reverseMe :: IO ()
reverseMe = do
    input <- getLine
    let output = unwords . reverse . words $ input
    print output

sayHi :: IO ()
sayHi = do
    putStr "What is your name? "
    name <- getLine
    when (name == "Bilbo") $ (do 
        putStrLn "So nice to meet you!"
        )
    putStrLn "What's up?"

calcAreas :: Num t => [t] -> [t] -> [t]
calcAreas = zipWith (*)

printAreas :: (Num t, Show t) => [t] -> [t] -> IO ()
printAreas lengths widths = mapM_ print $ calcAreas lengths widths

printDirection :: Char -> IO ()
printDirection dir
    | dir == 'u' = do print "UP"
    | dir == 'd' = do print "DOWN"
    | dir == 'l' = do print "LEFT"
    | otherwise  = do print "RIGHT"

printDirections :: [Char] -> IO ()
printDirections list = forM_ list (\dir -> do printDirection dir)
