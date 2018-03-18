# Generic catamorphisms [![Build Status](https://travis-ci.org/Lysxia/generic-recursion-schemes.svg?branch=master)](https://travis-ci.org/Lysxia/generic-recursion-schemes)

Derive folds on recursive types.

This library is compatible with the
[*recursion-schemes*](https://hackage.haskell.org/package/recursion-schemes)
library, and uses GHC Generics instead of Template Haskell to derive base
functors and recursion combinators.

`gcata` can also be used directly, without defining `Recursive` instances.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Generics
import Data.Functor.Foldable  -- recursion-schemes
import Generic.RecursionSchemes

data MyTree = Leaf Int | Node MyTree MyTree
  deriving Generic

type instance Base MyTree = GBase MyTree

instance Recursive MyTree where
  project = gproject

toList :: MyTree -> [Int]
toList = cata $ case_
  & match @"Leaf" (\n -> [n])
  & match @"Node" (\(ns, ms) -> ns ++ ms)
```
