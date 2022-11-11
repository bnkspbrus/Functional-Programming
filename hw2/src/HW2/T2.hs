{-# LANGUAGE TupleSections #-}

module HW2.T2 where

import HW2.T1

wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

wrapList :: a -> List a
wrapList a = a :. Nil

wrapFun :: a -> Fun i a
wrapFun a = F (const a)

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        = None
distOption (_, None)        = None
distOption (Some a, Some b) = Some (a, b)

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q e f g h) = Q (a, e) (b, f) (c, g) (d, h)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

extractPrioritised :: Prioritised a -> a
extractPrioritised (High a)   = a
extractPrioritised (Medium a) = a
extractPrioritised (Low a)    = a

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, p)    = High (a, extractPrioritised p)
distPrioritised (p, High b)    = High (extractPrioritised p, b)
distPrioritised (Medium a, p)  = Medium (a, extractPrioritised p)
distPrioritised (p, Medium b)  = Medium (extractPrioritised p, b)
distPrioritised (Low a, Low b) = Low (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> as, b :> bs) = (a, b) :> distStream (as, bs)

distList :: (List a, List b) -> List (a, b)
--distList = undefined
distList (Nil, _)      = Nil
distList (a :. as, l2) = concatList (mapList (a,) l2) (distList (as, l2))

concatList :: List a -> List a -> List a
concatList Nil l2       = l2
concatList (x :. xs) l2 = x :. concatList xs l2

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\i -> (f i, g i))
