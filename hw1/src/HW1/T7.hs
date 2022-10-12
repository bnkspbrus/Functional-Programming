module HW1.T7 where

data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x) xs = x :+ xs
  (<>) (x :+ xs) ys = x :+ (xs <> ys)

data Inclusive a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This a) (This b) = This (a <> b)
  (<>) (That a) (That b) = That (a <> b)
  (<>) (This a) (That b) = Both a b
  (<>) (That a) (This b) = Both b a
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)
  (<>) (Both a b) (This c) = Both (a <> c) b
  (<>) (Both a b) (That c) = Both a (b <> c)
  (<>) (This a) (Both b c) = Both (a <> b) c
  (<>) (That a) (Both b c) = Both  b (a <> c)

newtype DotString = DS String

instance Semigroup DotString where
  (<>) a (DS "") = a
  (<>) (DS "") a = a
  (<>) (DS s1) (DS s2) = DS (s1 ++ "." ++ s2)

instance Monoid DotString where
  mempty = DS ""
  
newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F f) (F g) = F (f . g)
  
instance Monoid (Fun a) where
  mempty = F id
  


