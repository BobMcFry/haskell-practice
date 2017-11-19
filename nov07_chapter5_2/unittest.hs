module Main where

import ChapterFive_Pt2
import Control.Exception
import Data.Int
import Test.HUnit
import Data.List

main :: IO ()
main = do
    testGetPaperTitle
    testgetPaperKeywords    
    testExtractAllKeywords
    testKeywordInList
    testExistsPaper
    testCountPapers

testGetPaperTitle = Control.Exception.catch (do
        putStrLn $ "\n------ Testing getPaperTitle ------"
        assertEqual "Failed to get Title" ["Computer Games as Motivation for Design Patterns"] (getPaperTitle paper)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testgetPaperKeywords = Control.Exception.catch (do
        putStrLn $ "\n------ Testing getPaperKeywords ------"
        assertEqual "Failed getting keywords" ["Design Patterns", "Games", "Pedagogy", "Java"] (getPaperKeywords paper)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testExtractAllKeywords = Control.Exception.catch (do
        putStrLn $ "\n------ Testing extractAllKeywords ------"
        assertEqual "Failed to extract all keywords" [["Test-driven learning","test-driven development","extreme programming","pedagogy","CS1"],
                                                        ["peer code review","behavior analysis","software quality assurance","computer science education","software engineering"],
                                                        ["Design Patterns","Games","Pedagogy","Java"],["Object-orientation","Design Patterns"],
                                                        ["CS education","Java","JUnit","unit testing","concurrent programming","tools","software engineering"],
                                                        ["Design Patterns","Game of Life","CS1","Laboratory"]] (extractAllKeywords papers)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testKeywordInList  = Control.Exception.catch (do
        putStrLn $ "\n------ Testing keywordInList ------"
        assertEqual "Failed to get keyword Games" True (keywordInList "Games" (getPaperKeywords paper))
        assertEqual "Failed to get keyword TDD" False (keywordInList "TDD" (getPaperKeywords paper))
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testExistsPaper  = Control.Exception.catch (do
        putStrLn $ "\n------ Testing existsPaper ------"
        assertEqual "Faild to see if a paper exits"[["Design Patterns","Games","Pedagogy","Java"],["Object-orientation","Design Patterns"],["Design Patterns","Game of Life","CS1","Laboratory"]] (existsPaper "Design Patterns" papers)
        assertEqual "Failed on TDD" [] (existsPaper "TDD" papers)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex


testCountPapers  = Control.Exception.catch (do
        putStrLn $ "\n------ Testing countPapers ------"
        assertEqual "Faied to count papers" 3 (countPapers "Design Patterns" papers)
        assertEqual "Failed to count no papers" 0 (countPapers "TDD" papers)
        putStrLn $ "   Passed all tests"
        ) handler
            where
                handler :: SomeException -> IO ()
                handler ex = putStrLn $ "Exception: " ++ show ex

