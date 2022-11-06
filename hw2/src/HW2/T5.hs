module HW2.T5 where

import qualified Control.Monad (ap)
import HW2.T1 (Annotated ((:#)), Except (Error, Success), mapAnnotated, mapExcept)
import HW2.T2
import HW2.T4 (Expr (..), Prim (..))

data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f ES {runES = res} = ES {runES = mapExcept (mapAnnotated f) . res}

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES {runES = \s -> wrapExcept (a :# s)}

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState es =
  ES
    { runES = \s -> case runES es s of
        (Error e) -> Error e
        (Success (es2 :# ns)) -> runES es2 ns
    }

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES {runES = \s -> wrapExcept (() :# f s)}

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES {runES = const (Error e)}

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = pure x
eval (Op (Add x y)) = do a <- eval x; b <- eval y; modifyExceptState (Add a b :); return (a + b)
eval (Op (Sub x y)) = do a <- eval x; b <- eval y; modifyExceptState (Sub a b :); return (a - b)
eval (Op (Mul x y)) = do a <- eval x; b <- eval y; modifyExceptState (Mul a b :); return (a * b)
eval (Op (Div x y)) = do a <- eval x; b <- eval y; guard b; modifyExceptState (Div a b :); return (a / b)
eval (Op (Abs x)) = do a <- eval x; modifyExceptState (Abs a :); return (abs a)
eval (Op (Sgn x)) = do a <- eval x; modifyExceptState (Sgn a :); return (signum a)

guard :: Double -> ExceptState EvaluationError s ()
guard 0 = throwExceptState DivideByZero
guard _ = ES {runES = \s -> wrapExcept (() :# s)}
