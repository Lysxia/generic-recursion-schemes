{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Data.Function ((&))
import GHC.Generics
import Test.Inspection

import Generic.RecursionSchemes

data Tree = End | Leaf Int | Node Tree Tree
  deriving Generic

toList_manual :: Tree -> [Int]
toList_manual End = []
toList_manual (Leaf n) = [n]
toList_manual (Node ns ms) = toList_manual ns ++ toList_manual ms

toList :: Tree -> [Int]
toList = gcata $ case_
  & match @"End"  (\() -> [])
  & match @"Leaf" (\n -> [n])
  & match @"Node" (\(ns, ms) -> ns ++ ms)

toList' :: Tree -> [Int]
toList' = gcata $ case_
  & match_ @"Node" (\ns ms -> ns ++ ms)
  & match_ @"Leaf" (\n -> [n])
  & match_ @"End"  []
  -- Branches in any order

toList1_manual :: Tree -> [Int]
toList1_manual t = go t [] where
  go End = id
  go (Leaf n) = (n :)
  go (Node ns ms) = go ns . go ms

toList1 :: Tree -> [Int]
toList1 t = go t [] where
  go :: Tree -> [Int] -> [Int]
  go = gcata $ case_
    & match_ @"End"  id
    & match_ @"Leaf" (\n -> (n :))
    & match_ @"Node" (\ns ms -> ns . ms)

size_manual :: Tree -> Int
size_manual End = 1
size_manual (Leaf _) = 1
size_manual (Node ns ms) = size_manual ns + (size_manual ms + 1)

size :: Tree -> Int
size = gcata $ foldr (+) 1

main :: IO ()
main = do
  let t = Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
  assertEq (toList  t) [0, 1, 2]
  assertEq (toList' t) [0, 1, 2]
  assertEq (size t) 5

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq a b = do
  if a == b then
    return ()
  else
    fail $ show a ++ " /= " ++ show b

inspect $ 'toList  ==- 'toList_manual
inspect $ 'toList' ==- 'toList_manual
inspect $ 'size    ==- 'size_manual
inspect $ 'toList1 ==- 'toList1_manual
