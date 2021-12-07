------------------------------Part 2a------------------------------
--Function Description:
{-The function steps takes 3 arguements, one for height 'h', width 'w' and the number of steps 'n',
 all being type Int and returns the result as a string. 
	-'step h w' replicates the char '*' w times and concatonates them into a string of '*'s and adds a '\n' at the end to make a new line'
		this is replicated h times to make up the height of 1 row of stairs and is concatonated
	-'z' is a list composing of 2*n Integers that make up the width of each row going up from w->n*w and back down
	-'map (step h) z' applies the step function described above and combines it onto each valie in the list z 
	-'concat $ map (step h) z' makes it all into a list of strings to be printed in the exercise-}

--Code:
steps :: Int -> Int -> Int -> String
steps h w n = concat $ map (step h) z
	where 
		step h w  = concat $ replicate h ((concat $ replicate w "*") ++ "\n")
		y = [w,2*w..n*w]
		z = y ++ (reverse y)
		

------------------------------Part 2b------------------------------
--Function Description:
{-The function steps takes 2 arguements, one for the dimensions 'dim' and one for the no. of flag duplicates 'j',
 both type Int and returns the result as a string. 
	-'chooser n dim i' decides for each char in a line whether it will be a '*' or a ' ' (space)
	-'(map (flagrow dim) [1..dim]' decides for flag row what the line what contain with '\n' at the end 
	-'map (chooser n dim) [1..dim]' maps the 'chooser n dim' onto every number from 1 to dim that will forms every line
	-'concat $ (map (flagrow dim) [1..dim])' concatonates every line to form a list of rows that make up 1 flag
	-'concat $ replicate j $ ...' replicactes every line and concatonates to make a list of j duplicates of the flag -}

--Code:
flagrow :: Int -> Int -> String
flagrow dim n = (concat $ map (chooser n dim) [1..dim]) ++ "\n" 
	where
		chooser n dim i
			| n == 1 || n == dim       = "*"
			| i == 1 || i == dim       = "*"
			| i == n || i == (dim-n+1) = "*"
			| otherwise = " "

flagpattern :: Int -> Int -> String
flagpattern dim j = concat $ replicate j $ concat $ (map (flagrow dim) [1..dim])

