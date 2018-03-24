{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -dsuppress-all #-}

import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Vinyl
import Test.Inspection

import Generic.RecursionSchemes
import Generic.RecursionSchemes.Internal

main :: IO ()
main = do
  assertEq (sum0  [1,2,3]) 6
  assertEq (sum0' [1,2,3]) 6

f0 :: ListF Int Int -> Int
f0 Nil = 0
f0 (Cons n m) = n + m :: Int
{-# INLINE f0 #-}

sum0_manual :: [Int] -> Int
sum0_manual [] = 0
sum0_manual (x : xs) = x + sum0_manual xs

sum0 :: [Int] -> Int
sum0 = cataList f0

sum0' :: [Int] -> Int
sum0' = cataList' f0

cataList :: (ListF b a -> a) -> [b] -> a
cataList f = (coerce :: ([Identity b] -> a) -> [b] -> a) (cataG (f . ListF))

cataList' :: (ListF b a -> a) -> [b] -> a
cataList' f = fix $ \cata_f -> f . fmap cata_f . projectList

f1 :: ListF Int (Int -> Int) -> Int -> Int
f1 Nil acc = acc
f1 (Cons n m) !acc = m (n + acc)

sum1_manual :: [Int] -> Int
sum1_manual xs = go xs 0
  where
    go :: [Int] -> Int -> Int
    go [] = \ acc -> acc
    go (x : xs') = \ !acc -> go xs' (x + acc)

{-
sum1_cata :: [Int] -> Int
sum1_cata xs = go xs 0
  where
    go = f . (fmap . fmap) go . e
    f Nothing acc = acc
    f (Just (h, t)) !acc = t (h + acc)
    e [] = Nothing
    e (x : xs) = Just (x, xs)
-}

sum1 :: [Int] -> Int
sum1 xs = cataList f1 xs 0

sum1' :: [Int] -> Int
sum1' xs = cataList' f1 xs 0

projectList :: [b] -> ListF b [b]
projectList [] = Nil
projectList (b : bs) = Cons b bs

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq a b = do
  if a == b then
    return ()
  else
    fail $ show a ++ " /= " ++ show b

newtype ListF b a = ListF (GBase [Identity b] a)
  deriving Functor

pattern Nil :: ListF b a
pattern Nil = ListF (GBase (Here (BaseConF RNil)))

pattern Cons :: b -> a -> ListF b a
pattern Cons h t =
  ListF (GBase (There (Here (BaseConF
    (Compose (Lazy (FromMaybeF (Identity h)))
      :& Compose (Lazy (FromMaybeF t))
      :& RNil)))))

{-# COMPLETE Nil, Cons #-}

inspect $ 'sum0  ==- 'sum0_manual
inspect $ 'sum0' ==- 'sum0_manual
-- TODO make these equal
inspect $ 'sum1  =/= 'sum1_manual
inspect $ 'sum1' =/= 'sum1_manual
