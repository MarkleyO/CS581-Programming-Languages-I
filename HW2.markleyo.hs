-- Owen Markley
-- CS 581 Fall 2020
module HW2 where

import HW1


--
-- * Part 1: Reverse Polish Notation
-- 

-- | Takes an expression and returns a string encoding of that expression in
--   Reverse Polish Notation (RPN).
--
--   >>> toRPN (Lit 3)
--   "3"
--
--   >>> toRPN e1
--   "2 3 4 * +"
--
--   >>> toRPN e2
--   "7 6 + 5 *"
--
--   >>> toRPN e3
--   "3 2 * 5 4 * +"
--
--   >>> elem (toRPN e4) ["8 7 9 * + 6 +", "8 7 9 * 6 + +"]
--   True
--   
toRPN :: Expr -> String
toRPN (Lit x) = show x
toRPN (Add x y) = (toRPN x) ++ " " ++ toRPN( y) ++ " +" 
toRPN (Mul x y) = (toRPN x) ++ " " ++ toRPN( y) ++ " *"


-- | Takes a string that is an RPN-encoded expression and produces the same
--   expression represented as an abstract syntax tree.
--
--   You can assume that your function will only be given valid RPN-encodings
--   of expressions. That is, it need not fail gracefully if it encounters an
--   error. However, if you would like to improve the error handling, you are
--   welcome to change the type of your function and the doctests.
--
--   >>> fromRPN "3"
--   Lit 3
--
--   >>> fromRPN "2 3 +"
--   Add (Lit 2) (Lit 3)
--
--   >>> fromRPN "2 3 4 + +"
--   Add (Lit 2) (Add (Lit 3) (Lit 4))
--
--   >>> all (\e -> e == fromRPN (toRPN e)) [e1,e2,e3,e4]
--   True
--

fromRPN :: String -> Expr
--modified some code found online
fromRPN = head . (foldl interpret []) . words
--foldl will take all the arguments given to it and perform the operation on the item
--therefore foldl will take the individual separated words obtained from words, and enter
--them into the function interpret. I'm not quite sure how head works here

	where -- where is somehow able to cause this to repeat
		interpret (a:b:c) "+" = (Add b a):c --a, b, and c indicates different segments. 
		--identifying a '+' will trigger the add function to be called on the first two
		--words that are on the stack, and this will be put in front of the rest
		interpret (a:b:c) "*" = (Mul b a):c --a similar thing is done for multiply
		interpret c a = (Lit (read a)):c -- This is able to append normal numbers as literals
		--to the stack



-- * Part 2: Syntactic Sugar
--

-- | Takes an expression and returns an expresion that evaluates to its
--  negation. Notice that this function does *not* evaluate the expression!
--   It returns a new expression that, when evaluated, will evaluate to the
--   negation of the original expression.
--
--   >>> eval e2
--   65
--
--   >>> eval (neg e2)
--   -65
--
neg :: Expr -> Expr 
neg x = Mul (x) (Lit (-1))

-- | Takes two expressions and returns an expression that evalautes to the
--   second expression subtracted from the first. Once again, note that the
--   return type is an expression.
--
--   >>> eval e1
--   14
--
--   >>> eval (sub e2 e1)
--   51
--
sub :: Expr -> (Expr -> Expr) 
sub x y = Add (x) (Mul (y) (Lit (-1)))