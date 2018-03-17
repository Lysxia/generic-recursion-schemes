{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Functor.Identity
import Data.Vinyl

import Generic.RecursionSchemes

main :: IO ()
main = do
  let f NilF = 0
      f (ConsF n m) = n + m :: Int
  assertEq (cataList  f [1,2,3]) 6
  assertEq (cataList' f [1,2,3]) 6

cataList :: (ListF b a -> a) -> [b] -> a
cataList f = cata (f . ListF) . (coerce :: [b] -> [Identity b])

cataList' :: (ListF b a -> a) -> [b] -> a
cataList' f = fix $ \cata_f -> f . fmap cata_f . projectList

projectList :: [b] -> ListF b [b]
projectList [] = NilF
projectList (b : bs) = ConsF b bs

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq a b = do
  if a == b then
    return ()
  else
    fail $ show a ++ " /= " ++ show b

newtype ListF b a = ListF (GBaseF [Identity b] a)
  deriving Functor

pattern NilF :: ListF b a
pattern NilF = ListF (Here (BaseConF RNil))

pattern ConsF :: b -> a -> ListF b a
pattern ConsF h t =
  ListF (There (Here (BaseConF (FromMaybeF (Identity h) :& FromMaybeF t :& RNil))))

{-# COMPLETE NilF, ConsF #-}
