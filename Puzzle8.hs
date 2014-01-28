module Puzzle8 where

--Classes for the puzzle
type Position = (Int, Int)

data Square = Empty | S Int
	deriving (Ord, Eq, Show)
type Row = [Square]
type Solution = [Char]
data Puzzle = Puzzle [Row] Solution | Invalid
	deriving (Show)

instance Eq Puzzle where
	(Puzzle r s) == (Puzzle ry sy)
		| (head $ head r) /= (head $ head ry)	= False
		| otherwise								= (r == ry)
	Invalid == Invalid				= True
	_ == _							= False

--Classes for the tree
--data Tree a = Leaf a | Branch [Tree a]

getPositionOfSquare :: Int -> Square -> Puzzle -> Position
getPositionOfSquare n s (Puzzle r _) = let indexes = map (\x -> length $ takeWhile (/=s) x) r in
										(minimum indexes, length $ takeWhile (==n) indexes)
	where
		findX _ [] _	= (-1)
		findX k (l:ls) n
			| l == k	= n
			| otherwise	= findX k ls (n+1)

puzzleToGoal n sp gp = breadthFirstSolution n sp gp
	 
swap2D :: (Int,Int) -> (Int,Int) -> [[Square]] -> [[Square]]
swap2D (x,y) (a,b) l
	= let afterFirstReplace = fstReplace in
		replaceNth y (replaceNth x val2 (fstReplace!!y)) fstReplace
		where
			fstReplace = replaceNth b (replaceNth a val1 (l!!b)) l
			val1 = head $ drop x $ head $ drop y l
			val2 = head $ drop a $ head $ drop b l
			
			--Used for replacing list elements
			replaceNth n newVal [] = []
			replaceNth n newVal (x:xs)
				 | n == 0 = newVal:xs
				 | otherwise = x : (replaceNth (n-1) newVal xs)
			
	 
--Movements for the puzzle
move :: Int -> Puzzle -> Char -> Int -> Int -> Puzzle
move n p c dx dy = let posOfEmpty = getPositionOfSquare n Empty p in
							moveHelper posOfEmpty p
				where
					moveHelper (x, y) (Puzzle r s)
						--test limits
						| y == 0 && dy < 0		= Invalid
						| y == (n-1) && dy > 0	= Invalid
						| x == 0 && dx < 0		= Invalid
						| x == (n-1) && dx > 0	= Invalid
						--copy the value down (overriding the Empty), then place the Empty one space in dy or dx direction
						| otherwise				= Puzzle (swap2D (x,y) (x+dx,y+dy) r) (s++[c])

moveDown :: Int -> Puzzle -> Puzzle
moveDown n p = move n p 'D' 0 (-1)

moveUp :: Int -> Puzzle -> Puzzle
moveUp n p = move n p 'U' 0 1

moveLeft :: Int -> Puzzle -> Puzzle
moveLeft n p = move n p 'L' 1 0
												
moveRight :: Int -> Puzzle -> Puzzle
moveRight n p = move n p 'R' (-1) 0

getSolution :: Puzzle -> Solution
getSolution (Puzzle r s) = s
getSolution Invalid = []

expand n ps = foldr (++) [] $ map (expandPuzzle n) ps
					
expandPuzzle _ Invalid	= []
expandPuzzle n p		= filter (\r -> (not . isRedundant) r && r /= Invalid) [moveDown n p, moveUp n p, moveLeft n p, moveRight n p]
	where
		isRedundant l =
			let l2c = drop (length sol - 2) sol in
				l2c == "DU" || l2c == "UD" || l2c == "RL" || l2c == "LR"
				where
					sol = getSolution l

breadthFirstSolution :: Int -> Puzzle -> Puzzle -> Puzzle
breadthFirstSolution n sp gp = searchSolution n [sp] gp
	where			
		searchSolution n ps gp
			-- if the solution is empty, search through an expanded version of the puzzles
			| solution == []	= searchSolution n (expand n ps) gp
			| otherwise			= head solution
			where
				solution = searchThrough ps gp
				
				searchThrough :: [Puzzle] -> Puzzle -> [Puzzle]
				searchThrough [] gp = []
				searchThrough (p:ps) gp
					| p == Invalid	= searchThrough ps gp
					| p == gp		= [p]
					| otherwise		= searchThrough ps gp
				
depthFirstSolution :: Int -> Puzzle -> Puzzle -> Puzzle
depthFirstSolution n sp gp = searchThrough n [sp] gp []
	where
		searchThrough :: Int -> [Puzzle] -> Puzzle -> [Puzzle] -> Puzzle
		searchThrough n (p:ps) gp e
			| p == gp						= p
			| not (isMember p e)			= searchThrough n (expandPuzzle n p ++ ps) gp (p:e)
			| otherwise						= searchThrough n ps gp e
			where
				isMember :: (Eq a) => a -> [a] -> Bool
				isMember k []		= False
				isMember k (x:xs)
					| x == k		= True
					| otherwise		= isMember k xs

outputBreadthFirstSolution n sp gp = 
	"Puzzle " ++ show (n) ++ ":\n" ++
	"Start State: " ++ show (sp) ++ "\n" ++
	"Goal State: " ++ show (gp) ++ "\n" ++
	"Solution: " ++ show (breadthFirstSolution 3 sp gp)
	
outputDepthFirstSolution n sp gp = 
	"Puzzle " ++ show (n) ++ ":\n" ++
	"Start State: " ++ show (sp) ++ "\n" ++
	"Goal State: " ++ show (gp) ++ "\n" ++
	"Solution: " ++ show (depthFirstSolution 3 sp gp)

main = do
	putStrLn "Breadth-First: \n"
	putStrLn $ (outputBreadthFirstSolution 1 testPuzzle1 goalPuzzle2) ++ "\n"
	putStrLn $ (outputBreadthFirstSolution 2 testPuzzle2 goalPuzzle) ++ "\n"
	putStrLn $ (outputBreadthFirstSolution 3 testPuzzle3 goalPuzzle) ++ "\n"
	putStrLn $ (outputBreadthFirstSolution 4 testPuzzle4 goalPuzzle) ++ "\n"
	putStrLn $ (outputBreadthFirstSolution 5 testPuzzle5 goalPuzzle) ++ "\n"
	putStrLn $ (outputBreadthFirstSolution 6 testPuzzle6 goalPuzzle) ++ "\n"
	putStrLn $ (outputBreadthFirstSolution 7 testPuzzle7 goalPuzzle) ++ "\n"
	putStrLn $ (outputBreadthFirstSolution 8 testPuzzle8 goalPuzzle) ++ "\n"
	putStrLn $ (outputBreadthFirstSolution 9 testPuzzle9 goalPuzzle) ++ "\n"
	putStrLn $ (outputBreadthFirstSolution 10 testPuzzle10 goalPuzzle) ++ "\n"
	
	putStrLn "\nDepth-First: \n"
	putStrLn $ (outputDepthFirstSolution 1 testPuzzle1 goalPuzzle2) ++ "\n"
	putStrLn "All other depth-first solutions do not finish.\nTrying puzzle 2 (will crash)..."
	putStrLn $ (outputDepthFirstSolution 2 testPuzzle2 goalPuzzle)
					
goalPuzzle = Puzzle [[Empty, S 1, S 2], [S 3, S 4, S 5], [S 6, S 7, S 8]] []
goalPuzzle2 = Puzzle [[S 1, S 2, S 3], [S 4, S 5, S 6], [S 7, S 8, Empty]] []

testPuzzle1 = Puzzle [[Empty, S 1, S 3], [S 4, S 2, S 5], [S 7, S 8, S 6]] []
testPuzzle2 = Puzzle [[S 1, S 8, S 3], [S 6, S 5, Empty], [S 4, S 7, S 2]] []
testPuzzle3 = Puzzle [[S 1, Empty, S 8], [S 5, S 7, S 6], [S 3, S 4, S 2]] []
testPuzzle4 = Puzzle [[S 4, S 1, S 5], [Empty, S 3, S 2], [S 7, S 8, S 6]] []
testPuzzle5 = Puzzle [[S 3, Empty, S 1], [S 8, S 4, S 2], [S 5, S 6, S 7]] []
testPuzzle6 = Puzzle [[S 8, S 2, S 4], [Empty, S 1, S 7], [S 3, S 6, S 5]] []
testPuzzle7 = Puzzle [[S 4, S 5, S 1], [S 8, S 7, S 6], [S 3, Empty, S 2]] []
testPuzzle8 = Puzzle [[S 6, S 3, S 5], [S 4, S 2, Empty], [S 7, S 1, S 8]] []
testPuzzle9 = Puzzle [[S 3, Empty, S 4], [S 5, S 6, S 2], [S 7, S 1, S 8]] []
testPuzzle10 = Puzzle [[S 1, Empty, S 4], [S 7, S 8, S 2], [S 5, S 6, S 3]] []