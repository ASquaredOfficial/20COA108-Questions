------------------------------Part 4------------------------------
--Function Description:
{-
The function lsplit takes 2 arguements, the first being a polymorphic list and the secong being a any polymorphic type being the same as the first. 
	-'(x,y)' first definined using the function span to return a list from its first element to the element before the function is false.
	-'splitOn char xs' returns list and appends the recursion of the remainder without its first element
	-'(filter (/=[]) (splitOn char string))' applies the filter function to remove all empty strings that could have been produced using the span function
	-'lsplit string char' maps the length function to create a list of the length of the lists-}

--Code:
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn char [] = []
splitOn char xs = x:(splitOn char (drop 1 y))
	where 
		(x,y) = span (/=char) xs


lsplit :: (Eq a) => [a] -> a -> [Int]
lsplit string char = map length (filter (/=[]) (splitOn char string))














