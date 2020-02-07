module HW2 where

import HW1


-- | A string rendering of the expression in Reverse Polish Notation (RPN).
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
toRPN (Lit a)  = show a
toRPN (Add a b)= toRPN a ++" "++ toRPN b ++" "++ "+"
toRPN (Mul a b)= toRPN a ++" "++ toRPN b ++ "*"


-- | Convert a string rendering of an expression in RPN into an expression
--   represented as an abstract syntax tree. You can assume that your function
--   will only be given valid strings, i.e. it need not fail gracefully if it
--   encounters an error.
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
fromRPN  a  = Lit a
fromRPN (a b)=reverse（a b）
-- the thinking way：
--1. it wants me to transfer the string to expr. 
--2. use the stack to thinking about this question. 
--3.reverse the string, which can makes them to the right order
--(ex "3 2 * 5 4 * + "->reverse: "+ * 4 5 * 2 3")
--4. case A: meet + use Add（ //HOWEVER i do not know how to add ")",
--   case B: meet * use Mul( 
--   case C: meet number use （Lit number)
--  --->expression will be Add ( Mul( (Lit 4) (Lit 5) ? Mul((Lit 2)(Lit 3)? ? 
--  --> the ? is for losting ")"

