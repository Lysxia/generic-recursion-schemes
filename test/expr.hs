{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -dsuppress-all #-}
{-# OPTIONS_GHC -funfolding-use-threshold=700 #-}

import Control.Applicative
import Control.Monad (guard, join)
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Coerce
import Data.Functor.Identity
import Data.Proxy
import Data.Type.Equality
import GHC.Generics (Generic)
import GHC.TypeLits
import Test.Inspection (inspect, (==-))
import Text.Read (readMaybe)

import Generic.RecursionSchemes

newtype Variable = V Int

data Expr
  = Var Variable
  | Int Int
  | Bool Bool
  | If Expr Expr Expr
  | (:+) Expr Expr
  | (:==) Expr Expr
  deriving Generic

data Ty = I | B
  deriving Eq

eval :: (Variable -> Int) -> Expr -> Maybe Expr
eval ctx = gcata $ case_
  (  #_Var --> Just . Int . ctx
  |. #_Int --> Just . Int
  |. #_Bool --> Just . Bool
  |. #_If --> (\a b c -> do
       Bool a' <- a
       if a' then b else c)
       -- Relying on Haskell's laziness here
  |. match_ @":+" (\a b -> do
       Int a' <- a
       Int b' <- b
       Just (Int (a' + b')))
  |. match_ @":==" (\a b -> do
       Int a' <- a
       Int b' <- b
       Just (Bool (a' == b')))
  )

eval_manual :: (Variable -> Int) -> Expr -> Maybe Expr
eval_manual ctx = go where
  go (Var v) = Just (Int (ctx v))
  go (Int n) = Just (Int n)
  go (Bool b) = Just (Bool b)
  go (If a b c) = do
    Bool a' <- go a
    if a' then go b else go c
  go (a :+ b) = do
    Int a' <- go a
    Int b' <- go b
    Just (Int (a' + b'))
  go (a :== b) = do
    Int a' <- go a
    Int b' <- go b
    Just (Bool (a' == b'))

typecheck :: Expr -> Maybe Ty
typecheck = gcata $ case_
  (  #_Var --> (\_ -> Just I)
  |. #_Int --> (\_ -> Just I)
  |. #_Bool --> (\_ -> Just B)
  |. #_If --> (\a b c -> do
       B <- a
       tyb <- b
       tyc <- c
       guard (tyb == tyc)
       Just tyb)
  |. match_ @":+" (\a b -> do
       I <- a
       I <- b
       Just I)
  |. match_ @":==" (\a b -> do
       I <- a
       I <- b
       Just B)
  )

typecheck_manual :: Expr -> Maybe Ty
typecheck_manual = go where
  go (Var _) = Just I
  go (Int _) = Just I
  go (Bool _) = Just B
  go (If a b c) = do
    B <- go a
    tyb <- go b
    tyc <- go c
    guard (tyb == tyc)
    Just tyb
  go (a :+ b) = do
    I <- go a
    I <- go b
    Just I
  go (a :== b) = do
    I <- go a
    I <- go b
    Just B

type Gen = StateT Int Rand

data Stream a = Stream a (Stream a)

newtype Rand a = Rand (Stream Int -> (a, Stream Int))

instance Functor Rand where
  fmap f (Rand m) = Rand $ \s -> let !(a, s') = m s in (f a, s')

instance Applicative Rand where
  pure a = Rand $ \s -> (a, s)
  Rand f_ <*> Rand x_ = Rand $ \s ->
    let !(f, s') = f_ s
        !(x, s'') = x_ s'
    in (f x, s'')

instance Monad Rand where
  return = pure
  Rand m_ >>= k = Rand $ \s ->
    let !(a, s') = m_ s
        Rand n_ = k a
    in n_ s'

genBool :: Gen Bool
genBool = lift (Rand $ \(Stream a as) -> (a `mod` 2 == 0, as))

genInt :: Int -> Gen Int
genInt n = lift (Rand $ \(Stream a as) -> (a `mod` n, as))

gen :: Ty -> Gen Expr
gen = ganaM genAlg

genAlg :: Ty -> Gen (GBase Expr Ty)
genAlg ty = do
  fuel <- get
  leaf_ <- genBool
  if leaf_ || fuel == 0 then
    genLeaf ty
  else do
    put (fuel - 1)
    genNode ty

genLeaf :: Ty -> Gen (GBase Expr t)
genLeaf I = join $ choose
  (  con_ @"Var" <$> genVar
  :? con_ @"Int" <$> genInt'
  :? Empty)
genLeaf B = con_ @"Bool" <$> genBool

genLeaf' :: Ty -> Gen Expr
genLeaf' = fmap gembed . genLeaf

genNode :: Ty -> Gen (GBase Expr Ty)
genNode I = choose
  (  con_ @":+" I I
  :? con_ @"If" B I I
  :? Empty)
genNode B = choose
  (  con_ @":==" I I
  :? con_ @"If" B B B
  :? Empty)

gen_manual :: Ty -> Gen Expr
gen_manual ty = do
  fuel <- get
  leaf_ <- genBool
  if leaf_ || fuel == 0 then
    genLeaf_manual ty
  else do
    put (fuel - 1)
    genNode_manual ty

genLeaf_manual :: Ty -> Gen Expr
genLeaf_manual I = join $ choose
  (  Var <$> genVar
  :? Int <$> genInt'
  :? Empty)
genLeaf_manual B = Bool <$> genBool

genNode_manual :: Ty -> Gen Expr
genNode_manual I = join $ choose
  (  liftA2 (:+) (gen_manual I) (gen_manual I)
  :? liftA3 If (gen_manual B) (gen_manual I) (gen_manual I)
  :? Empty)
genNode_manual B = join $ choose
  (  liftA2 (:==) (gen_manual I) (gen_manual I)
  :? liftA3 If (gen_manual B) (gen_manual B) (gen_manual B)
  :? Empty)

genVar :: Gen Variable
genVar = V <$> genInt 3
{-# INLINE genVar #-}

genInt' :: Gen Int
genInt' = genInt 10

data Choices n a where
  Empty :: Choices 0 a
  (:?) :: ((n == 0) ~ 'False) => a -> Choices (n-1) a -> Choices n a

infixr 3 :?

class Indexable n where
  index :: Int -> Choices n a -> a

instance {-# OVERLAPPING #-} Indexable 1 where
  index _ (a :? _) = a

instance ((n == 0) ~ 'False, Indexable (n-1)) => Indexable n where
  index 0 (a :? _) = a
  index i (_ :? as) = index (i-1) as

choose :: forall n a. (KnownNat n, Indexable n) => Choices n a -> Gen a
choose xs = do
  i <- genInt (fromIntegral (natVal @n Proxy))
  pure (index i xs)

-- | S-expression
data S = L [S] | Atom Token

data Token = TokV Int | TokN Int | TokTrue | TokFalse | TokIf | TokPlus | TokEq

parse :: S -> ContT r Maybe Expr
parse = ganaM $ \s ->
  case s of
    Atom (TokV v) -> pure (con_ @"Var" (V v))
    Atom TokFalse -> pure (con_ @"Bool" False)
    Atom TokTrue -> pure (con_ @"Bool" True)
    Atom (TokN n) -> pure (con_ @"Int" n)
    -- L [Atom TokIf, sa, sb, sc] -> pure (con_ @"If" sa sb sc)
    -- L [Atom TokPlus, sa, sb] -> pure (con_ @":+" sa sb)
    -- L [Atom TokEq, sa, sb] -> pure (con_ @":==" sa sb)
    _ -> lift Nothing

parse_manual :: S -> ContT r Maybe Expr
parse_manual s = case s of
  Atom (TokV v) -> pure (Var (V v))
  Atom TokFalse -> pure (Bool False)
  Atom TokTrue -> pure (Bool True)
  Atom (TokN n) -> pure (Int n)
  -- L [Atom TokIf, sa, sb, sc] -> If <$> go sa <*> go sb <*> go sc
  -- L [Atom TokPlus, sa, sb] -> (:+) <$> go sa <*> go sb
  -- L [Atom TokEq, sa, sb] -> (:==) <$> go sa <*> go sb
  _ -> lift Nothing
  where go = parse_manual

main = return ()

inspect $ 'eval ==- 'eval_manual
inspect $ 'typecheck ==- 'typecheck_manual
inspect $ 'genLeaf' ==- 'genLeaf_manual
-- inspect $ 'gen ==- 'gen_manual
-- inspect $ 'parse  ==- 'parse_manual

