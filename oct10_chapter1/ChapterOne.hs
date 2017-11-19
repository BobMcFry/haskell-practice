
-- author: Christian Heiden (cheiden@mymail.mines.edu)
-- date:   10-06-16

module ChapterOne where



    --------------------------------- PART ONE ---------------------------------

    -- negsInList: returns the number of negative elements in a list
    negsInList list = length [ x | x <- list, x < 0 ]

    -- oddsInList: returns the number of odd elements in a list
    oddsInList list = length [ x | x <- list, odd x ]

    -- capsInList: returns the number of capital letters in a list
    capsInList list = length [ x | x <- list, elem x ['A'..'Z'] ]

    -- sumOdd: returns the sum of the odd numbers in a list
    sumOdd list = sum [ x | x <- list, odd x ]

    

    --------------------------------- PART TWO ---------------------------------
    -- Given: receipt = [(3, 4), (5, 6)]

    -- getQuantities: returns a list of the quantities in the list
    getQuantities list = [ quant | (quant,price) <- list ]

    -- getPrices: returns a list of the prices in the list
    getPrices list = [ price | (quant,price) <- list ]

    -- getTotal: sums quantity*price for the list
    getTotal list = sum [ quant * price | (quant,price) <- list ]



    -------------------------------- PART THREE --------------------------------
    -- Given: list = [[1,2,3],[4,5]]

    -- doubleAll: doubles all elements
    doubleAll outerList = [ [ x*2 | x <- innerList ] | innerList <- outerList ]

    -- doubleFirstList: doubles all elements in only first list
    -- NOTE: Pattern matching is used since nested list comprehension does not provide
    -- a "nice" way to solve this problem
    doubleFirstList (first:_) = [ 2*x | x <- first ]

    -- doubleSecondList: doubles all elements in only the second list
    -- NOTE: Pattern matching is used since nested list comprehension does not provide
    -- a "nice" way to solve this problem
    doubleSecondList (_:second:_) = [ 2*x | x <- second ]

    -- sumListItems: returns a list whose elements are the sums of the values in
    -- the sublists
    sumListItems outerList = [ sum innerList | innerList <- outerList]
