module ChapterFive_Pt2 where

paper = [["Computer Games as Motivation for Design Patterns"],
           ["Design Patterns", "Games", "Pedagogy", "Java"]]

papers = [
    [["Test-Driven Learning: Intrinsic Integration of Testing into the CS/SE Curriculum"],
        ["Test-driven learning", "test-driven development","extreme programming", "pedagogy", "CS1"]],
    [["Process Improvement of Peer Code Review and Behavior Analysis of its Participants"],
        ["peer code review", "behavior analysis", "software quality assurance", 
            "computer science education", "software engineering"]],
    [["Computer Games as Motivation for Design Patterns"],
        ["Design Patterns", "Games", "Pedagogy", "Java"]],
    [["Killer Killer Examples for Design Patterns"],
        ["Object-orientation", "Design Patterns"]],
    [["Test-First Java Concurrency for the Classroom"],
        ["CS education", "Java", "JUnit", "unit testing", "concurrent programming",
        "tools", "software engineering"]],
    [["Teaching Design Patterns in CS1: a Closed Laboratory Sequence based on the Game of Life"],
        ["Design Patterns", "Game of Life", "CS1", "Laboratory"]]   
    ]

getPaperTitle :: [[a]] -> [a]
getPaperTitle p = head p

getPaperKeywords :: [[a]] -> [a]
getPaperKeywords p = last p

extractAllKeywords :: [[[a]]] -> [[a]]
extractAllKeywords [] = []
extractAllKeywords (a:rest) = (getPaperKeywords a) : extractAllKeywords rest

keywordInList :: Eq t => t -> [t] -> Bool
keywordInList kw list = elem kw list

existsPaper :: Eq t =>  t -> [[[t]]] -> [[t]]
existsPaper kw list = filter (keywordInList kw) (extractAllKeywords list)

countPapers :: (Eq t) => t -> [[[t]]] -> Int
countPapers kw list = length (existsPaper kw list)


