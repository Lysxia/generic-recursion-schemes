{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -dsuppress-all #-}
{-# OPTIONS_GHC -funfolding-use-threshold=600 #-}

import Data.Coerce
import GHC.Generics
import Data.Functor.Identity
import Test.Inspection

import Generic.RecursionSchemes
#if MIN_VERSION_base(4,10,0)
import Generic.RecursionSchemes.Labels
#endif

data Prop a
  = Var a
  | Not (Prop a)
  | And (Prop a) (Prop a)
  | Or  (Prop a) (Prop a)
  | If  (Prop a) (Prop a)
  | Iff (Prop a) (Prop a)
  deriving (Functor, Generic)

#if MIN_VERSION_base(4,10,0)
eval :: (a -> Bool) -> Prop a -> Bool
eval ctx = eval_ . (coerce :: Prop a -> Prop (Identity a)) where
  eval_ = cataG $ case_
    (  #_Var --> (ctx . runIdentity)
    |. #_Not --> not
    |. #_And --> (&&)
    |. #_Or  --> (||)
    |. #_If  --> (==>)
    |. #_Iff --> (==)
    )  -- Ugly syntax: overloaded labels must start with a lowercase character
#endif

eval' :: (a -> Bool) -> Prop a -> Bool
eval' ctx = eval_ . (coerce :: Prop a -> Prop (Identity a)) where
  eval_ = cataG $ case_
    (  match_ @"Var" (ctx . runIdentity)
    |. match_ @"Not" not
    |. match_ @"And" (&&)
    |. match_ @"Or"  (||)
    |. match_ @"If"  (==>)
    |. match_ @"Iff" (==)
    )  -- Ugly syntax: overloaded labels must start with a lowercase character

eval_manual :: (a -> Bool) -> Prop a -> Bool
eval_manual ctx = go where
  go (Var b)   = ctx b
  go (Not p)   = not (go p)
  go (And p q) = go p && go q
  go (Or  p q) = go p || go q
  go (If  p q) = go p ==> go q
  go (Iff p q) = go p == go q

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

example :: Prop Char
example =
  Not (Var 'P' `And` Var 'Q')
    `Iff` (Not (Var 'P') `Or` Not (Var 'Q'))

exampleCtx :: Char -> Bool
exampleCtx 'P' = False
exampleCtx 'Q' = True
exampleCtx _ = error "unbound variable"

main :: IO ()
main =
  assertEq (eval' exampleCtx example) True

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq a b = do
  if a == b then
    return ()
  else
    fail $ show a ++ " /= " ++ show b

#if MIN_VERSION_base(4,10,0)
inspect $ 'eval  ==- 'eval_manual
#endif
inspect $ 'eval' ==- 'eval_manual
