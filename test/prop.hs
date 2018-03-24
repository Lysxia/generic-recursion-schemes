{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -dsuppress-all #-}
{-# OPTIONS_GHC -funfolding-use-threshold=500 #-}

import Data.Coerce
import GHC.Generics
import Data.Functor.Identity
import Test.Inspection

import Generic.RecursionSchemes

data Prop a
  = Var a
  | Not (Prop a)
  | And (Prop a) (Prop a)
  | Or  (Prop a) (Prop a)
  | If  (Prop a) (Prop a)
  | Iff (Prop a) (Prop a)
  deriving (Functor, Generic)

eval :: (a -> Bool) -> Prop a -> Bool
eval ctx = eval_ . (coerce :: Prop a -> Prop (Identity a)) where
  eval_ = gcata $ case_
    & match_ @"Var" (ctx . runIdentity)
    & match_ @"Not" not
    & match_ @"And" (&&)
    & match_ @"Or"  (||)
    & match_ @"If"  (==>)
    & match_ @"Iff" (==)

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
  assertEq (eval exampleCtx example) True

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq a b = do
  if a == b then
    return ()
  else
    fail $ show a ++ " /= " ++ show b

inspect $ 'eval ==- 'eval_manual
