{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -funfolding-use-threshold=300 #-}
{-# OPTIONS_GHC -dsuppress-all #-}

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
toList = cataG $ case_
  (  match @"End"  (\() -> [])
  |. match @"Leaf" (\n -> [n])
  |. match @"Node" (\(ns, ms) -> ns ++ ms)
  )

toList' :: Tree -> [Int]
toList' = cataG $ case_
  (  match_ @"Node" (\ns ms -> ns ++ ms)
  |. match_ @"Leaf" (\n -> [n])
  |. match_ @"End"  []
  ) -- Branches in any order

toList1_manual :: Tree -> [Int]
toList1_manual t = go t [] where
  go End = id
  go (Leaf n) = (n :)
  go (Node ns ms) = go ns . go ms

toList1 :: Tree -> [Int]
toList1 t = go t [] where
  go :: Tree -> [Int] -> [Int]
  go = cataG $ case_
    (  match_ @"End"  id
    |. match_ @"Leaf" (\n -> (n :))
    |. match_ @"Node" (\ns ms -> ns . ms)
    )

size_manual :: Tree -> Int
size_manual End = 1
size_manual (Leaf _) = 1
size_manual (Node ns ms) = size_manual ns + (size_manual ms + 1)

size :: Tree -> Int
size = cataG $ foldr (+) 1

size1_manual :: Tree -> Int
size1_manual t = go t id where
  go End k = k 1
  go (Leaf _) k = k 1
  go (Node ns ms) k = go ns (\n -> go ms (\m -> k $! n + m + 1))

size1 :: Tree -> Int
size1 t = cataG alg t id where
  alg = caseDefault
    ( match @"Node" (\(ns, ms) k -> ns (\n -> ms (\m -> k $! n + m + 1)))
    ) (\_ k -> k 1)

size1' :: Tree -> Int
size1' t = cataG alg t id where
  alg = case_
    (  match_ @"End"  (\k -> k 1)
    |. match_ @"Leaf" (\_  k -> k 1)
    |. match_ @"Node" (\ns ms k -> ns (\n -> ms (\m -> k $! n + m + 1)))
    )

fib :: (Int, Int) -> [Int]
fib = anaG $ \(!a0, !a1) ->
  let a2 = a0 + a1 in con @":" (a0, (a1, a2))

fib' :: (Int, Int) -> [Int]
fib' = anaG $ \(!a0, !a1) ->
  let a2 = a0 + a1 in con_ @":" a0 (a1, a2)

fib_manual :: (Int, Int) -> [Int]
fib_manual (!a0, !a1) = a0 : fib_manual (a1, a2) where
  a2 = a0 + a1

fibTree :: Int -> Tree
fibTree = anaG $ \n ->
  if      n == 0 then con @"End" ()
  else if n == 1 then con @"Leaf" 1
  else                con @"Node" (n-1, n-2)

fibTree' :: Int -> Tree
fibTree' = anaG $ \n ->
  if      n == 0 then con_ @"End"
  else if n == 1 then con_ @"Leaf" 1
  else                con_ @"Node" (n - 1) (n - 2)

fibTree_manual :: Int -> Tree
fibTree_manual = go where
  go 0 = End
  go 1 = Leaf 1
  go n = Node (go (n - 1)) (go (n - 2))

main :: IO ()
main = do
  let t = Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)
  assertEq (toList  t) [0, 1, 2]
  assertEq (toList' t) [0, 1, 2]
  assertEq (size t) 5
  assertEq (take 6 (fib  (0, 1))) [0, 1, 1, 2, 3, 5]
  assertEq (take 6 (fib' (0, 1))) [0, 1, 1, 2, 3, 5]
  let sumTree = sum . toList
  assertEq (sumTree (fibTree  5)) 5
  assertEq (sumTree (fibTree' 5)) 5

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq a b = do
  if a == b then
    return ()
  else
    fail $ show a ++ " /= " ++ show b

inspect $ 'toList   ==- 'toList_manual
inspect $ 'toList'  ==- 'toList_manual
inspect $ 'size     ==- 'size_manual
inspect $ 'size1    ==- 'size1_manual
inspect $ 'size1'   ==- 'size1_manual
inspect $ 'toList1  ==- 'toList1_manual
inspect $ 'fib      ==- 'fib_manual
inspect $ 'fib'     ==- 'fib_manual
inspect $ 'fibTree  ==- 'fibTree_manual
inspect $ 'fibTree' ==- 'fibTree_manual
