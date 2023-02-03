module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)


data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  -- the value of Val is output from fromInteger
  fromInteger a = Val(fromInteger a)
{-   negate 0      = 0  -}
  negate      = UnApp Neg
 {-  (+) a 0       = a
  (+) 0 b       = b -}
  (+)       = BinApp Add
{-   (*) a 1       = a
  (*) 1 b       = b
  (*) a 0       = 0
  (*) 0 b       = 0 -}
  (*)       = BinApp Mul
-- Leave the following two undefined...
  signum        = undefined
  abs           = undefined

instance Fractional Exp where
  fromRational a = Val(fromRational a)
{-   (/) 0 a        = 0
  (/) a 1        = a -}
  (/)            = BinApp Div
-- Leave the following one undefined...
  recip          = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------
-- pair (x, y)
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key pairs = head [b | (a, b) <- pairs, a == key]

--without constraint then the data type range is too wide
op1 :: Floating a => [(UnOp, a->a)]
op1 = [(Neg, negate),
       (Sin, sin),
       (Cos, cos),
       (Log, log)]

op2 :: Floating a =>[(BinOp, a->a->a)]
op2 = [(Add, (+)),
       (Mul, (*)),
       (Div, (/))]
       
eval :: Exp -> Env -> Double 
eval (Val a) _ = a 
eval (Id a) env = lookUp a env
eval (UnApp a b) env = lookUp a op1 (eval b env)
eval (BinApp a b c) env = lookUp a op2 (eval b env) (eval c env)


{- data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show) -}

fromUnOp :: UnOp -> String
fromUnOp Neg = "-"
fromUnOp Sin = "sin"
fromUnOp Cos = "cos"
fromUnOp Log = "log"

{- data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show) -}

fromBinOp :: BinOp -> String
fromBinOp Add = "+"
fromBinOp Mul = "*"
fromBinOp Div = "/"

{- data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show) -}

fromExp :: Exp -> String
fromExp (Val a) = show a
fromExp (Id a) = a
fromExp (UnApp a b) = fromUnOp a ++ "("++fromExp b++")"
fromExp (BinApp a b c) = "("++(fromExp b ++ fromBinOp a ++fromExp c)++")"

showExp :: Exp -> String
showExp = fromExp 


--the more tedious and redundant method 

{- diff :: Exp -> String -> Exp
diff (Val a) _ = Val 0.0 
diff (Id a) str
        | a == str = Val 1.0 
        | otherwise = Val 0.0 
diff (UnApp op a) str
        | op == Sin = BinApp Mul (UnApp Cos a) chain
        | op == Cos = UnApp Neg (BinApp Mul (UnApp Sin a) chain)
        | op == Log = BinApp Div chain a
        | otherwise = UnApp op chain
        where chain = diff a str 
diff (BinApp op a b ) str 
        | op == Add = BinApp Add chain1 chain2
        | op == Mul = BinApp Add (BinApp Mul a chain2) (BinApp Mul chain1 b)
        | op == Div = BinApp Div (BinApp Add (BinApp Mul chain1 b)(UnApp Neg (BinApp Mul a chain2))) (BinApp Mul c c)
        where chain1 = diff a str
              chain2 = diff b str -}

--simplier 
diff :: Exp -> String -> Exp
diff (Val exp) _ = 0 
diff (Id exp) str
        | exp == str = 1 
        | otherwise = 0

diff (UnApp op exp) str
        | op == Sin = cos exp * chain
        | op == Cos = negate (sin exp * chain)
        | op == Log = chain/exp
        | otherwise = negate chain
        where chain = diff exp str 

diff (BinApp op exp1 exp2 ) str        
        | op == Add = chain1+chain2
        | op == Mul = (exp1*chain2)+(chain1*exp2)
        | op == Div = ((chain1*exp2)-(exp1*chain2))/(exp2*exp2)
        where chain1 = diff exp1 str
              chain2 = diff exp2 str 

maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp val n = sum item
        where item = zipWith3 (\x y z -> (x*y)/z) diffList powerX factItem
              diffList = map (`eval` [("x", 0.0)]) 
                        (take n (iterate (`diff` "x") exp))
              powerX = 1:iterate (val*) val
              factItem = scanl (*) 1 [1..]

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
