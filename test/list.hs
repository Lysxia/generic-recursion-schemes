{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Functor.Identity
import Data.Vinyl
import Test.Inspection

import Generic.RecursionSchemes

main :: IO ()
main = do
  assertEq (sum0  [1,2,3]) 6
  assertEq (sum0' [1,2,3]) 6

f0 :: ListF Int Int -> Int
f0 Nil = 0
f0 (Cons n m) = n + m :: Int

sum0 :: [Int] -> Int
sum0 = cataList f0

sum0' :: [Int] -> Int
sum0' = cataList' f0

cataList :: (ListF b a -> a) -> [b] -> a
cataList f = (coerce :: ([Identity b] -> a) -> [b] -> a) (gcata (f . ListF))

cataList' :: (ListF b a -> a) -> [b] -> a
cataList' f = fix $ \cata_f -> f . fmap cata_f . projectList

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
pattern Nil = ListF (Here (BaseConF RNil))

pattern Cons :: b -> a -> ListF b a
pattern Cons h t =
  ListF (There (Here (BaseConF (FromMaybeF (Identity h) :& FromMaybeF t :& RNil))))

{-# COMPLETE Nil, Cons #-}

inspect $ 'sum0 === 'sum0'
