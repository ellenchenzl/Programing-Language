module HW5 where

import Prelude hiding (Num)


-- int	::=	(any integer)	integers
 			
-- bool	::=	true   |   false	booleans
 			
-- reg	::=	A   |   B	register names
 			
-- expr	::=	int	integer literal
-- |	bool	boolean literal
-- |	reg	load from register
-- |	expr + expr	integer addition
-- |	expr <= expr	less than or equal to
-- |	not expr	boolean negation
-- |	expr & expr	boolean conjunction
 			
-- stmt	::=	reg := expr	store to register
-- |	if expr	conditional statement
-- then prog	
-- else prog	
-- end	
 			
-- prog	::=	ε  |  stmt ; prog	sequence of statements


-- 1  Encode the abstract syntax for the language as a set of Haskell types and data types.
type Prog = [Stmt]

data Reg 
   = A 
   | B
   deriving (Eq,Show)

data Expr 
   = PushN Int 
   | PushB Bool
   | PushR Reg
   | Add Expr Expr
   | Les Expr Expr
   | Not Expr
   | And Expr Expr
   deriving (Eq,Show)

 data Stmt 
 	 =Sto Reg Expr
 	|if Expr 
   then Prog 
   else Prog
   deriving (Eq,Show)
--2  Encode the AST of the example program above as a Haskell value.
 	
-- A := 3;          { set register A to 3 }
-- B := A+2;        { set register B to A+2, which will be 5 }
-- if A <= B then   { if A is less than or equal to B ... }
--   A := A + A;    { ... then double A }
-- else
--   B := B + B;    { ... otherwise double B }
-- end;
-- B := A+B;        { set B to A+B }
--                  { at the end of the program: A=6, B=11 }


ex1 = [Sto A (PushN 3), Sto B (Add (PushR A) (PushN 2)), If (Les (PushR A) (PushR B)) [Sto A (Add (PushR A) (PushR A))] [Sto B (Add (PushR B) (PushR B))], Sto B (Add (PushR A) (PushR B))]



------------
-- Write a Haskell function sumFiveOrLess :: [Int] -> Prog that takes a list of integers
-- and returns a program in our object language that sums up all of the integers in the list
-- that are less than or equal to 5. The final result should be stored in register A.
------------
sumFiveOrLess :: [Int] -> Prog
sumFiveOrLess []     = [Store A (Lit 0)]
sumFiveOrLess (x:xs) = sumFiveOrLess xs ++ [Condition (Less (Lit x) (Lit 5)) [Store A (Add (Regi A) (Lit x))] [Store A (Regi A)] ]






--3  Write a Haskell function sumFiveOrLess :: [Int] -> Prog that takes a list of integers and returns 
--a program in our object language that sums up all of the integers in the list that are less than or 
--equal to 5. The final result should be stored in register A.

sumFiveOrLess :: [Int] -> Prog
sumFiveOrLess []= [Sto A (PushN 0)]
sumFiveOrLess (i:is)=sumFiveOrLess is ++ [If (Les (PushN i) (PushN 5)) [Sto A (PushR A)] [Sto A (Add (PushR A) (PushN i))]]
              





