module HW4Part2 where

-- Refactor the syntax of the language to eliminate the possibility of type errors. 
-- The new syntax should be able to express all of the type correct programs that 
-- could be represented before and none of the type incorrect ones. 
-- Write the grammar of the new syntax in a comment in your file.

-- I think the only thing that I should need to do here, is to split the expr data type
-- into tht two separate ones, in which one uses bool, and one uses int? 

type Prog = [Stmt]

type Reg = A
	| B
	| C

-- since there is already an int type in Haskell, I think I can consider it already encoded
data IntExpr = Int
	| Load Reg
	| Add IntExpr IntExpr
	deriving(Eq, Show)

data BoolExpr
	| LessEqual IntExpr IntExpr
	| Not BoolExpr
	deriving(Eq, Show)

-- I think this should make it so that I have the full functionality of the previous setting
-- but now I have one version for each type of expr?
data Stmt = RegStore Reg IntExpr
	| RegStore Reg BoolExpr
	| Contitional IntExpr Prog Prog
	| Contitional BoolExpr Prog Prog
	| Loop Prog
	| BreakLoop
	deriving(Eq, Show)