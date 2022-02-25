module HW4Part1 where

-- 1. Encode the abstract syntax for the language as a set of Haskell types and data types.

-- int	::=	(any integer)	integers
 			
-- reg	::=	A  |  B  |  R	register names
 			
-- expr	::=	int	integer literal
-- |	reg	load from register
-- |	expr + expr	integer addition
-- |	expr <= expr	less than or equal to
-- |	not expr	boolean negation
 			
-- stmt	::=	reg := expr	store to register
-- |	if expr	conditional statement
-- then prog	
-- else prog	
-- end	
-- |	do prog	loop until break
-- end	
-- |	break	break out of a loop
 			
-- prog	::=	ε  |  stmt ; prog	sequence of statements
type Prog = [Stmt]

type Reg = A
	| B
	| C

-- since there is already an int type in Haskell, I think I can consider it already encoded
data Expr = Int
	| Load Reg
	| Add Expr Expr
	| LessEqual Expr Expr
	| Not Expr
	deriving(Eq, Show)

data Stmt = RegStore Reg Expr
	| Contitional Expr Prog Prog
	| Loop Prog
	| BreakLoop
	deriving(Eq, Show)

-- This should match the general idea and format of the examples in the lecture

-- 2. Encode the AST of the example program above as a Haskell value.
-- I'm not sure how to make sure that any of this actually works, so I'm just assuming that some
-- of it is probably right

-- each level of brackets is like an indentation level here
encoded :: Prog
encoded = 	[RegStore A (Int 7), RegStore B (Int 9), RegStore R (Int 0), Loop --at this point, loop invokes another program
				[Contitional (LessEqual (Load A) (Int 0)) -- if
					([BreakLoop]) -- then
					([RegStore R (Add (Load R) (Load B)) , RegStore A (Add (Load A) (Int -1))])]] -- else

-- 3. Define a function while :: Expr -> Prog -> Stmt that defines a standard while loop as syntactic sugar.
while :: Expr -> Prog -> Stmt
while = 


-- 4. Problem 4
sumFromTo :: Int -> Int -> Prog
sumFromTo x y = 	[RegStore A (x), RegStore B (y) RegStore R (Int 0), Loop
						[Conditional (LessEqual (Load A) (Load B)) -- if
							([RegStore R (Add (Load R) (Load A)), RegStore A (Add (Load A) (Int 1))]) -- then
							([BreakLoop])]] --else
-- this should store the integers into a and b. as long as a is less than or equal to b, a will be
-- added to a third register R and then register a will be incremented by one