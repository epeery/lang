{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ITree where

import Control.Applicative (liftA, liftA2)
import Control.Comonad.Cofree
import Data.Functor.Compose

data ITree r = forall a. Node r (a -> r) [ITree a]

deriving instance Functor ITree

instance Show r => Show (ITree r) where
  show (Node r f as) = "Node " <> show r <> " " <> show ((f <$>) <$> as)

data A a = forall e. A (e -> a) e

deriving instance Functor A

type Test = Cofree (Compose A [])

t :: Test Int
t = 1 :< (Compose $ A (const $ [2 :< (Compose $ A id [])]) "hi")
