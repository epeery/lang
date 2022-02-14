{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Box where

import Control.Applicative
import Control.Category
import ITree
import Positionable
import Prelude hiding (id, (.))

newtype Box e a = Box {unBox :: e -> a}
  deriving (Functor, Applicative, Monad)

type Container = ITree (Layout Box)

instance Num (Box e Double) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = liftA (abs)
  signum = liftA (signum)
  fromInteger = pure . fromInteger

instance Fractional (Box e Double) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance Positionable Box

instance Containable Box where
  screen = Box fst
  container = Box snd

instance Category Box where
  id = Box id
  Box f . Box g = Box $ f . g
