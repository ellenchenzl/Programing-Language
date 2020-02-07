module HW5 where

import Prelude hiding (Num)

-- Observe that programs in our language may have type errors. 
--For example, the following statements all have type errors:

-- A := not 3;             { not can only be applied to booleans }
-- B := 4 + true;          { can't add an integer and a boolean }
-- A := true;              { registers may only contain integers }-
-- if 2+3 then else end;   { condition must be a boolean }
-- (Note that our syntax allows empty then and else branches, so the third line is not a syntax error.)

-- 4 Refactor the syntax of the language to eliminate the possibility of type 
--   errors. The new syntax should be able to express all of the type correct 
--   programs that could be represented before and none of the type incorrect 
--   ones. Write the grammar of the new syntax in a comment in your file.

-- int	::=	(any integer)	integers
 			
-- bool	::=	true   |   false	booleans
 			
-- reg	::=	A   |   B	register names
 			
-- expri	::=	int             integer literal
-- 		     |	expri + expri	integer addition
-- 		     |	reg	            load from register
--            | expri <= expri integer addition

-- exprb	::=	bool	        bool literal
-- 		     |	not exprb	    boolean negation 
-- 		     |	exprb & exprb	boolean conjunction


-- stmt	::=	reg := expri	store to register
--       |	if exprb      conditional statement
--          then prog	
--          else prog	
-- end	
 			
-- prog	::=	ε  |  stmt ; prog	sequence of statements

-- 5 Encode the new abstract syntax as a set of Haskell types and data types.

type Prog = [Stmt]

data Reg 
   = A 
   | B
   deriving (Eq,Show)

data Expri 
   = PushN Int 
   | PushR Reg
   | Les Expri Expri
   | Add Expri Expri
   deriving (Eq,Show)

data Exprb
   = PushB Bool
   | Les Expri Expri
   | Not Exprb
   | And Exprb Exprb
   deriving (Eq,Show)

 data Stmt 
 	=Sto Reg Expri
 	|if Exprb 
 	then Prog 
 	else Prog
   deriving (Eq,Show)

