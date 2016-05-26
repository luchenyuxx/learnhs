{-# LANGUAGE FlexibleInstances #-}
module Calc where

-- import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add l r) = (eval l) + (eval r)
eval (Mul l r) = (eval l) * (eval r)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp lit add mul

class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

class HasVars a where
  var :: String -> a
-- instance Expr ExprT where
--   lit = Lit
--   add = Add
--   mul = Mul

-- reify :: ExprT -> ExprT
-- reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit i | i <= 0 = False
        | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit n = Mod7 $ mod n 7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a+b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a*b) 7

instance Expr Program where
  lit i= [PushI i]
  add a b = a ++ b ++ [Add]
  mul a b = a ++ b ++ [Mul]

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

compile :: String -> Maybe Program
compile = parseExp lit add mul
