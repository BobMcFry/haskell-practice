import System.IO
import Data.Char

startsWithVowel :: String -> Bool
startsWithVowel string = elem (head string) ['a','e','i','o','u']

getEnding :: String -> String
getEnding string
    | startsWithVowel string = "way"
    | otherwise = (take 1 string) ++ "ay"

getBeginning :: String -> String
getBeginning string
    | startsWithVowel string = string
    | otherwise = tail string

pigLatin :: String -> String
pigLatin string = getBeginning string ++ getEnding string

convertToPigLatin :: String -> String
convertToPigLatin input = mergeWordsAndLines . map convertLine $ splitLinesAndWords input

splitLinesAndWords :: String -> [[String]]
splitLinesAndWords string = map words $ lines string

convertLine :: [String] -> [String]
convertLine line = map pigLatin line

mergeWordsAndLines :: [[String]] -> String
mergeWordsAndLines lns = unlines $ map unwords lns

piggly :: IO ()
piggly = do 
    hSetBuffering stdout LineBuffering
    interact convertToPigLatin

caesar :: Int -> Char -> Char
caesar seed char 
    | elem char ['a'..'z'] = chr ((mod (((ord char)-97) + seed) 26) + 97)
    | elem char ['A'..'Z'] = chr ((mod (((ord char)-65) + seed) 26) + 65)
    | otherwise = char

encrypt :: Int -> IO ()
encrypt seed = do
    hSetBuffering stdout LineBuffering
    interact (\line -> map (caesar seed) line)

encryptFile :: Int -> IO ()
encryptFile seed = do
    contents <- readFile "encryptMe.txt"
    let result = lines $ map (caesar seed) contents
    mapM_ (putStrLn) result






