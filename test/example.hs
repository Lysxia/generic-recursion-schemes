{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

import GHC.Generics
import Data.Functor.Foldable  -- recursion-schemes
import Generic.RecursionSchemes

data MyTree = Leaf Int | Node MyTree MyTree
  deriving Generic

type instance Base MyTree = GBase MyTree

instance Recursive MyTree where
  project = projectG

instance Corecursive MyTree where
  embed = embedG

toList :: MyTree -> [Int]
toList = cataG $ case_
  (  match @"Leaf" (\n -> [n])
  |. match @"Node" (\(ns, ms) -> ns ++ ms)
  )

main :: IO ()
main = print (toList (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))

-- Output: [0,1,2]
