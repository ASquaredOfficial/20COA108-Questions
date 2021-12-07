------------------------------Part 3------------------------------
--Function Description:
{-The function steps takes 2 arguements, one for peron one and one for person two, both type String and returning the result as a string. 
	-'delete x (y:ys)' first defining the delete function by checking the first element with x to see if it is equal then returning the rest of the list.
		If the element is not equal to to x then it checks the next elements recursivly 
	-'(\\)' assigned using foldr (delete) so it applies delete onto every element in the list/string
	-'(x,y)'assigned by filtering out the spaces from the xs and ys input
	-'z' checks the remainder of length of x \\ y
	-'person xs ys' dependent on the remainder 'z' appropriate response printed
	-'compatibility xs ys' prints the relation ship between person xs and ys and vice versa-}

--Code:
delete :: (Eq a) => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys)
	| x==y = ys 
	| otherwise = y:(delete x ys)

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) = foldr (delete)

compatibility:: String -> String -> String
compatibility xs ys = person xs ys ++ " and " ++ person ys xs
	where 
		person xs ys 
			| z == 1 = xs++" loves "++ys
			| z == 2 = xs++" is physically attracted to "++ys
			| z == 3 = xs++" hates "++ys
			| z == 0 = xs++" is indifferent to "++ys
			where 
				z = (length $ x \\ y) `mod` 4
				(x,y) = (filter (/=' ') xs, filter (/=' ') ys)














