{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import Data.Function ((&))
import GHC.Generics

import Generic.RecursionSchemes

data Tree = Leaf Int | Node Tree Tree
  deriving Generic

toList :: Tree -> [Int]
toList = cata $ case_
  & match @"Leaf" (\n -> [n])
  & match @"Node" (\(ns, ms) -> ns ++ ms)

toList' :: Tree -> [Int]
toList' = cata $ case_
  & match_ @"Leaf" (\n -> [n])
  & match_ @"Node" (\ns ms -> ns ++ ms)

main :: IO ()
main = do
  let t = Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
  assertEq (toList  t) [0, 1, 2]
  assertEq (toList' t) [0, 1, 2]

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq a b = do
  if a == b then
    return ()
  else
    fail $ show a ++ " /= " ++ show b

