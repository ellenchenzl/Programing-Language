module HW9 where

import Prelude hiding (and,or,not,pred,succ,fst,snd,either)

import DeBruijn
import Church


--
-- * Church-encoded lists.
--

-- ** Encoding

-- | Pretend Haskell's Int is restricted to natural numbers.
type Nat = Int

-- | A Church-encoded list is represented by a sum of encodings for the 'nil'
--   case and 'cons' case. Nil is represented by the identity function.
nil :: Exp
nil = App inL (Abs (Ref 0))

-- | A smart constructor for building a 'cons' node, encoded as a pair of the
--   head and tail of the list.
cons :: Exp -> Exp -> Exp
cons h t = App inR (app2 pair h t)

-- | Encode a Haskell list of natural numbers in lambda calculus as a
--   Church-encoded list.
encodeList :: [Nat] -> Exp
encodeList []    = nil
encodeList (h:t) = cons (num h) (encodeList t)


-- ** Sum

-- | Write a lambda calculus function that computes the sum of a
--   Church-encoded list of natural numbers.
sumList :: Exp
sumList = Abs (app3 either helper1 helper2 (Ref 0))

helper1 :: Exp
helper1 = Abs (num 0)
helper2 = Abs (app2 add (App fst (Ref 0)) (App sumList (App (snd) (Ref 0))))

-- | For testing your lambda-calculus encoded sum function. Should return a
--   Church-encoded number.
runSum :: [Nat] -> Exp
runSum l = eval (App sumList (encodeList l)) 

-- | A function for testing your sum function. Checks to see if the lambda
--   calculus encoding returns the same value as Haskell's sum function.
--
--   >>> testSum [] 
--   True
--
--   >>> testSum [2,3,4]
--   True
--
testSum :: [Nat] -> Bool
testSum l = num (sum l) == runSum l


-- ** Map

-- | Write a lambda calculus function that maps a function (given as its
--   first argument) over a Church-encoded list (second argument).
mapList :: Exp
mapList = undefined
-- In this mapList, I understand that mapList takes two expression, one is 
-- the enccodeList， one is the opreation and the number.
-- so in the mapList, the firs argument is consist by the App, Opreation and num
-- mapList = Abs (App Opreation num)(encodeList[])
-- I would write the opreation in this way
-- Opreation :: Exp
-- Opreation n = Abs (help name (Ref 0))
--         where help name e？？？？？
-- I cannot give the pattern match of Opreation Function


-- | Map function tests.
--
--   >>> :{
--     eval (encodeList [3,4,5]) ==
--     eval (app2 mapList (App add two) (encodeList [1,2,3]))
--   :}
--   True
--   
--   >>> :{
--     eval (encodeList [3,6,9]) ==
--     eval (app2 mapList (App mult three) (encodeList [1,2,3]))
--   :}
--   True