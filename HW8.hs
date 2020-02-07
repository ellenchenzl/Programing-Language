module HW8 where

import Prelude hiding (and,or,not,pred,succ,fst,snd,either)

import DeBruijn
import Church


--
-- * Part 1: Nameless lambda calculus
--

-- | λx. (λx.x) x
--
--   >>> eval ex1
--   Abs (Ref 0)
--
ex1 :: Exp
ex1 = App(Abs(Abs (Ref 0)))(Ref 0)

-- | (λxy.xz) z z
--
--   >>> eval ex2
--   App (Ref 0) (Ref 0)
--
ex2 :: Exp
ex2 = App(App(Abs(App(Abs(Ref 1))(Ref 2)))(Ref 0))(Ref 0)

-- | λx. (λxy.yx) x z
--
--   >>> eval ex3
--   Abs (App (Ref 1) (Ref 0))
--   
ex3 :: Exp
ex3 = Abs(app2(abs2(App(Ref 0)(Ref 1))) (Ref 0)(Ref 1))


-- | Is the given nameless lambda calculus term a closed expression? That is,
--   does it contain no free variables?
--
--   >>> closed (Ref 0)
--   False
--
--   >>> closed (Abs (Ref 0))
--   True
--
--   >>> closed (Abs (App (Ref 0) (Ref 1)))
--   False
--
--   >>> closed (Abs (App (Abs (App (Ref 0) (Ref 1))) (Ref 0)))
--   True
--
-- judgement :: Int->Exp->Bool
-- judgement (App l r)= judgement i + judgement 
-- judgement (App (Abs e) r)= 
-- judgement (Abs e)= False
-- judgement (Ref e)= if e=0 then False
--                   |otherwise True

-- closed :: Exp -> Bool
-- closed = judgement i

--In this question, I understand I should use λ format to solve it
--For Example, in λ （λ 1 2）0 0， for every λ we should check the same 
--stage number ex:λ （λ 1 2）0 0,then judge them is less and equal to zero
--                ^         ^ ^
--if it is equal less to zero， closed function will +1,return True
--if it is not, the closed function will +0, return False;
--Then check the next stage
--in this case, it turns to  λ （λ 1 2）0 0
--                               ^ ^ ^  
--And only App Exp Exp is the same stage; the Abs Exp is not in same stage                   
--However, my problem is I do not know howto peel of the function level
--by level. I according the step function in Debrujin.hs. It does not 
--work.

-- * Part 2: Church pair update functions
--

-- | Write a lambda calculus function that replaces the first element in a
--   Church-encoded pair. The first argument to the function is the original
--   pair, the second is the new first element.
--
--   >>> :{
--     eval (app2 pair true (num 3)) ==
--     eval (app2 setFst (app2 pair (num 2) (num 3)) true)
--   :}
--   True
--
setFst :: Exp
setFst = abs2 ( App (Ref 1) (abs3(app2 (Ref 0) (Ref 3) (Ref 1))))

-- | Write a lambda calculus function that replaces the second element in a
--   Church-encoded pair. The first argument to the function is the original
--   pair, the second is the new second element.
--
--   >>> :{
--     eval (app2 pair (num 2) true) ==
--     eval (app2 setSnd (app2 pair (num 2) (num 3)) true)
--   :}
--   True
--
setSnd :: Exp
setSnd = abs2 ( App (Ref 1) (abs3(app2 (Ref 0) (Ref 2) (Ref 3))))


--
-- * Part 3: Church encoding a Haskell program
--

-- | Pretend Haskell's Int is restricted to Nats.
type Nat = Int

-- | A simple data type with three cases.
data Foo = N Nat | B Bool | P Nat Bool
  deriving (Eq,Show)

-- | Compute a numeric value from a Foo.
--   (This is just an arbitrary function.)
bar :: Foo -> Nat
bar (N n)     = n * 3
bar (B True)  = 1
bar (B False) = 0
bar (P n b)   = n + if b then 1 else 0

-- | Write a Haskell function that converts a Foo into a
--   lambda calculus term.
encodeFoo :: Foo -> Exp
encodeFoo (N n)      = num(n*3)
encodeFoo (B True)   = one
encodeFoo (B False)  = zero
encodeFoo (P n True) = num(n+1)
encodeFoo (P n False)= num n
-- | Write the bar function as a lambda calculus term.
barExp :: Exp
barExp = App fix (abs2(Ref 0))

-- | Run your encoded bar function on an encoded Foo value.
runBar :: Foo -> Exp
runBar x = eval (App barExp (encodeFoo x))

-- | A function for testing encodeFoo and barExp. Checks to see if the lambda
--   calculus encoding returns the same number as the given value function.
--
--   >>> testFooBar (N 4)
--   True
--
--   >>> testFooBar (B True)
--   True
--
--   >>> testFooBar (B False)
--   True
--
--   >>> testFooBar (P 5 True)
--   True
--
--   >>> testFooBar (P 5 False)
--   True
--
testFooBar :: Foo -> Bool
testFooBar x = num (bar x) == runBar x