import Data.List
import System.Directory

data Matrix a = Matrix [[a]] | Nil
              deriving (Eq)


instance (Show a) => Show (Matrix a) where
	show (Matrix a) = printString (Matrix a)
	show (Nil) = "Nil"

instance (Num a)=>Num (Matrix a) where
	(*) _ Nil = Nil
	(*) Nil _ = Nil
	(*) (Matrix x) (Matrix y) = multiplyMatrix (Matrix x) (Matrix y)
	(+) (Matrix x) (Matrix y) = addMatrix (Matrix x) (Matrix y)
	(+) _ Nil = Nil
	(+) Nil _ = Nil
	(-) (Matrix x) (Matrix y) = subMatrix (Matrix x) (Matrix y)
	(-) _ Nil = Nil
	(-)	Nil _ = Nil


main = do 
	putStrLn "\n/-----------------------------------\\\n|Welcome to my custom Matrix Library|"
	putStrLn "\\-----------------------------------/\n<*>Current supported operations:\n"
	putStrLn "-> repeatMatrix row col value : Creates a row*col matrix initialised to value\n"
	putStrLn "-> printMatrix (Matrix matrix) : Prints the matrix onto terminal\n"
	putStrLn "-> getDimension (Matrix matrix) : Returns a tupple of the form (row,col) of the matrix\n"
	putStrLn "-> generateMatrix row col genFunc : returns a row*col matrix where each value is generated with the genFunc (\\(i,j)-> ...)\n"
	putStrLn "-> identityMatrix dim : returns a dim*dim Identity Matrix\n"
	putStrLn "-> scalarMult scalar (Matrix matrix) : returns a new matrix which is the result of the applied Scalar Multiplication \n"
	putStrLn "-> diagonalList dim ls : given a list of elements, returns a dim*dim matrix where the diagonal entries index the list\n"
	putStrLn "-> permMatrix dim : prints the list of the permutations of the Identity on dim\n"
	putStrLn "-> linearise (Matrix matrix) : given a matrix object, returns the 1 dimensional array form of the matrix\n"
	putStrLn "-> fromList row col ls : given a list ls of atleast row*col elements, return an row*col matrix\n"
	putStrLn "-> getRow r (Matrix matrix) : returns the rth row of the argument matrix\n"
	putStrLn "-> getCol c (Matrix matrix) : returns the cth column of the argument matrix\n"
	putStrLn "-> multiplyMatrix (Matrix m1) (Matrix m2) : returns the resultant matrix of m1 * m2 if multiplication is defined, else []\n"
	putStrLn "-> transposeMatrix (Matrix matrix) : returns the tranposed form of matrix\n"
	putStrLn "-> saveMatrix (Matrix matrix) : writes the matrix to a new text file in the Current Directory\n"
	putStrLn "-> equalMatrix (Matrix a) (Matrix b) : returns whether 2 matrices are equal\n"
	putStrLn "-> isTriangularMatrix (Matrix a) : returns whether a matrix is triangular \n"
	putStrLn "-> mapRow mapFunc row (Matix a) : returns a new matrix where the specified row is updated according to mapping function\n"
	putStrLn "-> scaleRow row scaleFactor (Matrix a) : a generalised application of mapRow where the map func is scalar multiplication(row wise)\n"
	putStrLn "-> mapCol mapFunc col (Matix a) : returns a new matrix where the specified col is updated according to mapping function\n"
	putStrLn "-> scaleCol col scaleFactor (Matrix a) : a generalised application of mapCol where the map func is scalar multiplication(column wise)\n"
	putStrLn "-> extendTo value newRow newCol (Matrix a) : given a row*col Matrix a, extendTo increase the dimensions to the newly provided, inserting value in new slots\n"
	putStrLn "-> switchRows r1 r2 (Matrix a) : returns a new matrix where rows r1 and r2 are switched\n"
	putStrLn "-> switchRows c1 c2 (Matrix a) : returns a new matrix where columns c1 and c2 are switched\n"
	putStrLn "-> getDiag (Matrix a) : returns a list of the diagonal entries in the matrix\n"
	putStrLn "-> trace (Matrix a) : returns the trace (sum of diagonal entries) \n"
	putStrLn "-> det (Matrix a): returns the determinant of Matrix a \n"
	putStrLn "-> minorMatrix r c (Matrix a) : returns the minor matrix of a where the rth row and cth column is elimnated\n"
	putStrLn "-> matrixOfMinor (Matrix a) : returns the matrix where the (i,j) indexed value represents the determinant of the minorMatrix of i j\n"
	putStrLn "-> cofactorMatrix (Matrix a) : returns the cofactorMatrix of (Matrix a)\n"
	putStrLn "-> adjointMatrix (Matrix a) : returns the adjointMatrix of (Matrix a)\n"
	putStrLn "-> inverseMatrix (Matrix a) : returns the inverseMatrix of (Matrix a)\n"


removeMatrix (Matrix a) = a

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

repeatMatrix :: Int -> Int -> a ->  (Matrix a)
repeatMatrix row col value | row > 0 && col > 0 = Matrix (replicate row (replicate col value))
						   | otherwise = Nil


printMatrix :: Show a => Matrix a -> IO()
printMatrix Nil = putStr ("~Undefined Matrix")
printMatrix (Matrix matrix) = putStr(printString (Matrix matrix) )

printString :: Show a => Matrix a -> String
printString Nil = ""
printString (Matrix matrix) = ("\n->("++show(length(matrix))++","++show(length(matrix!!0))++") matrix\n"++ printMatrixHelper (Matrix matrix) 0)

printMatrixHelper :: Show a => Matrix a -> Int -> String
printMatrixHelper (Matrix matrix) row | row < length(matrix) = show (matrix!!row) ++ "\n" ++ printMatrixHelper (Matrix matrix) (row+1)
					   				  | otherwise = "\n"

buildRow :: (Int,Int) -> Int -> Int -> ((Int,Int) -> a) -> [a]
buildRow (row,col) travR travC func | travC < col = [func (travR+1,travC+1)] ++ buildRow (row,col) travR (travC+1) func
								 	| travR+1 < row = buildRow (row,col) (travR+1) 0 func
								 	| otherwise = []

getDimension :: Matrix a -> (Int,Int)
getDimension Nil = (0,0)
getDimension (Matrix matrix) = (length(matrix),length(matrix!!0))

generateMatrix :: Int -> Int -> ((Int,Int) -> a) -> Matrix a
generateMatrix row col genFunc =  Matrix (splitEvery col (buildRow (row,col) 0 0 genFunc))


--identityMatrix :: Int -> Matrix Int
identityMatrix dim | dim > 0 = generateMatrix dim dim (\(i,j) -> fromIntegral (fromEnum (i == j)))
				   | otherwise = Nil


scalarMult :: (Num a) => a -> Matrix a -> Matrix a
scalarMult _ Nil = Nil
scalarMult scalar (Matrix matrix) = generateMatrix (length matrix) (length (matrix!!0)) (\(i,j)-> ( (matrix!!(i-1))!!(j-1) ) * scalar)


diagonalList :: Num a => Int -> [a] -> Matrix a 
diagonalList _ [] = Nil
diagonalList n ls | n <= len = generateMatrix n n (\(i,j)-> (fromIntegral (fromEnum (i==j))*(ls!!(i-1))))
				  | n > len = diagonalList n (ls ++ (replicate diff zero))
				  where
				  	len = length ls
				  	diff = n - len
				  	zero = (ls!!0) - (ls!!0)


printMatrixList :: Show a =>  [[[a]]] -> IO()
printMatrixList [] = putStrLn "~Empty List"
printMatrixList (x:xs) = putStrLn ( (intercalate " " (map (\x -> printString (Matrix x)) (x:xs))) ++ show(length(x:xs))++" permutations")


permMatrix :: Int -> IO()
permMatrix 0 = putStrLn "~Matrix of dimension 0 DNE"
permMatrix dim = printMatrixList (permutations (removeMatrix (identityMatrix dim)) )



linearise :: Matrix a -> [a]
linearise Nil = []
linearise (Matrix (x:xs)) = concat (x:xs)


fromList :: Int -> Int -> [a] -> Matrix a
fromList _ _ [] = Nil
fromList row col (x:xs) | length(x:xs) >= row*col = generateMatrix row col (\(i,j)-> (x:xs)!!((i-1)*col+(j-1)))
						| otherwise = Nil

indexMatrix :: (Num a) => Int -> Int -> Matrix a -> a
indexMatrix row col (Matrix matrix) | row > 0 && row <= length(matrix) && col > 0 && col <= length (matrix!!0) = (matrix!!(row-1))!!(col-1)
					       			| otherwise = zero
					       			where 
					       				zero = ((matrix!!0)!!0)-((matrix!!0)!!0)



getRow :: Int -> Matrix a -> [a]
getRow _ Nil = []
getRow row (Matrix matrix) | row < length(matrix) = matrix!!row
				  		   | otherwise = []

getCol :: Int -> Matrix a -> [a]
getCol _ Nil = []
getCol col (Matrix matrix) | col < length(matrix!!0) = getColHelper matrix 0 col
				  		   | otherwise = []

getColHelper :: [[a]] -> Int -> Int -> [a]
getColHelper matrix row col | row < length(matrix) = [(matrix!!row)!!col] ++ getColHelper matrix (row+1) col
							| otherwise = []


multiplyMatrix :: (Num a)=> Matrix a -> Matrix a -> Matrix a
multiplyMatrix Nil _ = Nil
multiplyMatrix _ Nil = Nil
multiplyMatrix (Matrix m1) (Matrix m2) | c1 == r2 = fromList r1 c2 (multiplyMatrixHelper 0 0 m1 m2)
					 				   | otherwise = Nil
					where
						(r1,c1) = getDimension (Matrix m1)
						(r2,c2) = getDimension (Matrix m2)

multiplyMatrixHelper :: (Num a) => Int -> Int -> [[a]]->[[a]] -> [a]
multiplyMatrixHelper row col m1 m2 | col < length(m2!!0) = [sum(zipWith (*) m1Row m2Col)] ++ multiplyMatrixHelper row (col+1) m1 m2
								   | row < length(m1) = multiplyMatrixHelper (row+1) 0 m1 m2
								   | otherwise = []
										where
											m1Row = getRow row (Matrix m1)
											m2Col = getCol col (Matrix m2)


addMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
addMatrix Nil _ = Nil
addMatrix _ Nil = Nil
addMatrix (Matrix m1) (Matrix m2) | r1 == r2 && c1 == c2 = Matrix (addMatrixHelper m1 m2 0)
								  | otherwise = Nil
									where
										(r1,c1) = getDimension (Matrix m1)
										(r2,c2) = getDimension (Matrix m2)

addMatrixHelper :: (Num a) => [[a]] -> [[a]] -> Int -> [[a]]
addMatrixHelper m1 m2 row | row < length(m1) = [zipWith (+) m1Row m2Row] ++ addMatrixHelper m1 m2 (row+1)
						  | otherwise = []
						  where 
						  	m1Row = getRow row (Matrix m1)
						  	m2Row = getRow row (Matrix m2)

subMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
subMatrix Nil _ = Nil
subMatrix _ Nil = Nil
subMatrix (Matrix m1) (Matrix m2) = addMatrix (Matrix m1) (scalarMult (-1) (Matrix m2))



transposeMatrix :: Matrix a -> Matrix a
transposeMatrix Nil = Nil
transposeMatrix (Matrix matrix) =
	let linear = linearise (Matrix matrix) 
		in
		generateMatrix col row (\(i,j)-> linear!! ((j-1)*row + (i-1)))
		where
			(row,col) = getDimension (Matrix matrix)


equalMatrix :: (Eq a) => Matrix a -> Matrix a -> Bool
equalMatrix _ Nil = False
equalMatrix Nil _ = False
equalMatrix (Matrix a) (Matrix b) = linearise (Matrix a) == linearise (Matrix b)


isTriangularMatrix :: (Eq a,Num a) => Matrix a -> Bool
isTriangularMatrix Nil = False
isTriangularMatrix (Matrix a) | rows /= cols = False
							  | rows == cols && rows == 1 = True -- 1 by 1 Matrix
							  | ((a!!0)!!1) == zero =  isTriangularMatrixHelper (Matrix a) 0 -- Upper triangular Check (0's on the right)
							  | otherwise = isTriangularMatrixHelper (transposeMatrix (Matrix a)) 0
							  where
							  	(rows,cols) = getDimension (Matrix a)
							  	zero = ((a!!0)!!0) - ((a!!0)!!0)

isTriangularMatrixHelper ::(Eq a,Num a)=> Matrix a -> Int -> Bool 
isTriangularMatrixHelper (Matrix a) travRow | travRow < (row -1) = ((replicate lengthSlice zero) == getSublist curRow (travRow+1))  && isTriangularMatrixHelper (Matrix a) (travRow+1)
											| otherwise = True
										where
										(row,col) = getDimension (Matrix a) 
										zero = ((a!!0)!!0) - ((a!!0)!!0)
										curRow = getRow travRow (Matrix a)
										lengthSlice = col - (travRow +1)

getSublist :: [a] -> Int -> [a]
getSublist [] _ = []
getSublist (x:xs) skip | skip == 0 = (x:xs)
					   | otherwise = getSublist xs (skip-1)

extendTo _ _ _ Nil = Nil
extendTo value newRow newCol (Matrix a) | newRow <= row && newCol <= col = Matrix a
										| otherwise = generateMatrix newRow newCol (\(i,j) -> (fromEnum (i <= row && j<= col))*(indexMatrix i j (Matrix a)) + (fromEnum ((i>row || j>col)) * value))
										where
											(row,col) = getDimension (Matrix a)



mapRow :: (a -> a) -> Int -> Matrix a -> Matrix a
mapRow _ _ Nil = Nil
mapRow mapFunc row (Matrix a) | row <= rows && row > 0 = Matrix ((init above) ++ [newRow] ++ below)
							  | otherwise = Matrix a
							where
								(above,below) = splitAt row a
								(rows,cols) = getDimension (Matrix a)
								newRow = map mapFunc (getRow (row-1) (Matrix a))


scaleRow _ _ Nil = Nil
scaleRow row scaleFactor (Matrix a) = mapRow (\x -> x*scaleFactor) row (Matrix a)

mapCol :: (a -> a) -> Int -> Matrix a -> Matrix a
mapCol _ _ Nil = Nil
mapCol mapFunc col (Matrix a) | col <= cols && col > 0 = transposeMatrix (mapRow mapFunc col (transposeMatrix (Matrix a)))
							  | otherwise = Matrix a
							where
								(rows,cols) = getDimension (Matrix a)	
scaleCol _ _ Nil = Nil
scaleCol col scaleFactor (Matrix a) = mapCol (\x -> x*scaleFactor) col (Matrix a)						 


switchRows _ _ Nil = Nil
switchRows r1 r2 (Matrix a) | r1 == r2 = Matrix a
							| r1 <= rows && r1 > 0 && r2 <= rows && r2 > 0 = generateMatrix rows cols  (\(i,j) -> (fromEnum (i == r1) * r2Row!!(j-1)) + (fromEnum (i == r2) * r1Row!!(j-1)) + (fromEnum (i /= r1 && i /= r2) * indexMatrix i j (Matrix a)))
							where
								r1Row = getRow (r1-1) (Matrix a)
								r2Row = getRow (r2-1) (Matrix a)
								(rows,cols) = getDimension (Matrix a)


switchCols _ _ Nil = Nil
switchCols c1 c2 (Matrix a) | c1 == c2 = Matrix a
							| c1 <= cols && c1 > 0 && c2 <= cols && c2 > 0 = transposeMatrix (switchRows c1 c2 (transposeMatrix (Matrix a)))
							where
							(rows,cols) = getDimension (Matrix a) 

getDiag :: (Num a)=>Matrix a -> [a]
getDiag Nil = []
getDiag (Matrix a) = getDiagHelper (Matrix a) 0

getDiagHelper :: (Num a)=>Matrix a -> Int -> [a]
getDiagHelper (Matrix a) trav | trav < length(a) = [indexMatrix (trav+1) (trav+1) (Matrix a)] ++ getDiagHelper (Matrix a) (trav+1)
							  | otherwise = []


trace (Matrix a) = sum (getDiag (Matrix a))


det :: (Eq a,Num a)=>Matrix a -> a
det (Matrix a) | isTriangularMatrix (Matrix a) = product (getDiag (Matrix a))
			   | rows == 2 && cols == 2 = (indexMatrix 1 1 mat)*(indexMatrix 2 2 mat) - (indexMatrix 1 2 mat)*(indexMatrix 2 1 mat)
			   | rows == cols = detHelper (Matrix a) 0
			   | otherwise = zero 
				where
					(rows,cols) = getDimension (Matrix a)
					mat = Matrix a 
					zero = ((a!!0)!!0) - ((a!!0)!!0)

detHelper (Matrix a) travC | travC < cols = (indexMatrix 1 (travC+1) (Matrix a)) * (det  minor)*((-1)^(travC)) + (detHelper (Matrix a) (travC+1))
						   | otherwise = zero
							where
								(rows,cols) = getDimension (Matrix a)
								minor = minorMatrix 1 (travC+1) (Matrix a)
								zero = ((a!!0)!!0) - ((a!!0)!!0)



minorMatrix :: Num a => Int -> Int -> Matrix a -> Matrix a
minorMatrix _ _ Nil = Nil
minorMatrix r c (Matrix a) | r<= rows && r > 0 && c <= cols && c > 0 = fromList (rows-1) (cols-1) (minorHelper (Matrix a) (r,c) 0 0)
						   | otherwise = Matrix a
							where 
								(rows,cols) = getDimension (Matrix a)
								line = linearise (Matrix a)
								
minorHelper :: Num a => Matrix a -> (Int,Int) -> Int -> Int -> [a]
minorHelper (Matrix a) (limR,limC) travR travC | travR == (limR-1) = minorHelper (Matrix a) (limR,limC) (travR+1) 0
											   | travC /= (limC -1) && travC < cols = [indexMatrix (travR+1) (travC+1) (Matrix a)] ++ minorHelper (Matrix a) (limR,limC) travR (travC+1)
											   | travC == (limC-1)  = minorHelper (Matrix a) (limR,limC) travR (travC +1)
											   | travC == cols && travR+1 < rows = minorHelper (Matrix a) (limR,limC) (travR+1) 0
											   | otherwise = []
												where
												(rows,cols) = getDimension (Matrix a) 

matrixOfMinor :: (Num a,Eq a) => Matrix a -> Matrix a
matrixOfMinor Nil = Nil
matrixOfMinor (Matrix a) | rows == cols = fromList rows cols (matrixOfMinorHelper (Matrix a) (0,0))
						 | otherwise = Nil
						where
							(rows,cols) = getDimension (Matrix a)

matrixOfMinorHelper :: (Num a,Eq a) => Matrix a -> (Int,Int) -> [a]
matrixOfMinorHelper (Matrix a) (r,c) | c < cols = [det (minorMatrix (r+1) (c+1) (Matrix a))] ++  matrixOfMinorHelper (Matrix a) (r,c+1)
									 | r < rows = matrixOfMinorHelper (Matrix a) (r+1,0)
									 | otherwise = []  
									where
										(rows,cols) = getDimension (Matrix a)
										zero = ((a!!0)!!0) - ((a!!0)!!0)

cofactorMatrix :: (Num a,Eq a) => Matrix a -> Matrix a
cofactorMatrix Nil = Nil
cofactorMatrix (Matrix a) | rows == cols = generateMatrix rows cols (\(i,j) -> (indexMatrix i j minorList) * (-1)^(i+j))
						  | otherwise = Nil
							where
								(rows,cols) = getDimension (Matrix a)
								zero = ((a!!0)!!0) - ((a!!0)!!0)
								minorList = matrixOfMinor (Matrix a)
adjointMatrix :: (Num a,Eq a) => Matrix a -> Matrix a
adjointMatrix Nil = Nil
adjointMatrix (Matrix a) = transposeMatrix (cofactorMatrix (Matrix a))


inverseMatrix Nil = Nil
inverseMatrix (Matrix a) | rows == cols && det(Matrix a) /= 0 = scalarMult detReciprocal (adjointMatrix (Matrix a)) 
						 | otherwise = Nil
							where
								(rows,cols) = getDimension (Matrix a)
								zero = ((a!!0)!!0) - ((a!!0)!!0)
								determinant = det (Matrix a)
								detReciprocal = 1 / (determinant)



header = "             Matrix Library Output \n===================================================\n"
footer = "\n==================================================="
saveMatrix (Matrix matrix) = writeFile ("/Users/abdulrafaenoor/Desktop/Personal/Programming/Haskell"++"/mat.txt") (header ++ printString (Matrix matrix) ++ footer)


