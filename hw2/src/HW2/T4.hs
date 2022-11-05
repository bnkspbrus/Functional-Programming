module HW2.T4 where

import qualified Control.Monad
import HW2.T1

data State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f S {runS = rs} = S {runS = \s -> let (a :# ns) = rs s in f a :# ns}

wrapState :: a -> State s a
wrapState a = S {runS = (a :#)}

joinState :: State s (State s a) -> State s a
joinState S {runS = rs} = S {runS = \s -> let (S {runS = rs2} :# ns) = rs s in rs2 ns}

modifyState :: (s -> s) -> State s ()
modifyState f = S {runS = \s -> () :# f s}

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum
  deriving (Show)

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  x - y = Op (Sub x y)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval (Val x) = pure x
eval (Op (Add x y)) = do a <- eval x; b <- eval y; S {runS = \s -> (a + b) :# Add a b : s}
eval (Op (Sub x y)) = do a <- eval x; b <- eval y; S {runS = \s -> (a - b) :# Sub a b : s}
eval (Op (Mul x y)) = do a <- eval x; b <- eval y; S {runS = \s -> (a * b) :# Mul a b : s}
eval (Op (Div x y)) = do a <- eval x; b <- eval y; S {runS = \s -> (a / b) :# Div a b : s}
eval (Op (Abs x)) = do a <- eval x; S {runS = \s -> abs a :# Abs a : s}
eval (Op (Sgn x)) = do a <- eval x; S {runS = \s -> signum a :# Sgn a : s}
