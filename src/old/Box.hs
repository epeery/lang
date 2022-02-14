{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Box where

import Control.Monad.Reader
import Data.Proxy
import Vector

data Alignment = SpaceBetween | SpaceAround

type View a = Reader (Box 'Constrained 'Constrained a) (Box 'Constrained 'Constrained a)

data Box h v a where
  Box :: Box 'Empty 'Empty a
  Merge :: [Box 'Constrained 'Constrained a] -> Box 'Constrained 'Constrained a
  Row :: (CanStart h, HasSize h) => [Box h v a] -> Box 'Empty 'Empty a
  Col :: (CanStart v, HasSize v) => [Box h v a] -> Box 'Empty 'Empty a
  SetWidth :: CanSize h => a -> Box h v a -> Box (AddSize h) v a
  SetHeight :: CanSize v => a -> Box h v a -> Box h (AddSize v) a
  SetTop :: CanStart v => a -> Box h v a -> Box h (AddStart v) a
  SetBottom :: CanEnd v => a -> Box h v a -> Box h (AddEnd v) a
  SetLeft :: CanStart h => a -> Box h v a -> Box (AddStart h) v a
  SetRight :: CanEnd h => a -> Box h v a -> Box (AddEnd h) v a
  SetCenterX :: CanMiddle h => a -> Box h v a -> Box (AddMiddle h) v a
  SetCenterY :: CanMiddle v => a -> Box h v a -> Box h (AddMiddle v) a
  Unwrap :: Box h v (Val a) -> Box h v a
  IfElse :: Val Bool -> Box h v a -> Box h v a -> Box h v a

instance Show a => Show (Box h v a) where
  show Box = "Box"
  show (Merge bs) = "Merge (" <> show bs <> ")"
  show (Row bs) = "Row (" <> show bs <> ")"
  show (Col bs) = "Col (" <> show bs <> ")"
  show (SetWidth n b) = "Width (" <> show n <> " " <> show b <> ")"
  show (SetHeight n b) = "Height (" <> show n <> " " <> show b <> ")"
  show (SetTop n b) = "Top (" <> show n <> " " <> show b <> ")"
  show (SetBottom n b) = "Bottom (" <> show n <> " " <> show b <> ")"
  show (SetLeft n b) = "Left (" <> show n <> " " <> show b <> ")"
  show (SetRight n b) = "Right (" <> show n <> " " <> show b <> ")"
  show (SetCenterX n b) = "CenterX (" <> show n <> " " <> show b <> ")"
  show (SetCenterY n b) = "CenterY (" <> show n <> " " <> show b <> ")"
  show (Unwrap b) = "Unwrap (" <> show b <> ")"
  show (IfElse p a b) = "IfElse (" <> show p <> ")" <> " (" <> show a <> ") (" <> show b <> ")"

deriving instance Functor (Box h v)

deriving instance Foldable (Box h v)

deriving instance Traversable (Box h v)

data Val a where
  Val :: a -> Val a
  Plus :: Val a -> Val a -> Val a
  Minus :: Val a -> Val a -> Val a
  Times :: Val a -> Val a -> Val a
  Div :: Val a -> Val a -> Val a
  Abs :: Val a -> Val a
  Signum :: Val a -> Val a
  -- Eq :: Val a -> Val a -> Val Bool
  -- Gt :: Val a -> Val a -> Val Bool
  -- Lt :: Val a -> Val a -> Val Bool
  -- Not :: Val Bool -> Val Bool
  -- Or :: Val Bool -> Val Bool -> Val Bool
  -- And :: Val Bool -> Val Bool -> Val Bool
  GetWidth :: HasSize h => Box h v a -> Val a
  GetHeight :: HasSize v => Box h v a -> Val a
  GetTop :: HasStart v => Box h v a -> Val a
  GetBottom :: HasEnd v => Box h v a -> Val a
  GetLeft :: HasStart h => Box h v a -> Val a
  GetRight :: HasEnd h => Box h v a -> Val a
  GetCenterX :: HasMiddle h => Box h v a -> Val a
  GetCenterY :: HasMiddle v => Box h v a -> Val a

deriving instance Show a => Show (Val a)

deriving instance Functor Val

deriving instance Foldable Val
deriving instance Traversable Val

instance (Fractional a, Eq a, GetVal a) => Eq (Val a) where
  a == b = getVal a == getVal b

instance (Fractional a, Ord a, GetVal a) => Ord (Val a) where
  a <= b = getVal a <= getVal b

instance Fractional a => Num (Val a) where
  a + b = Plus a b
  a - b = Minus a b
  a * b = Times a b
  abs = Abs
  signum = Signum
  fromInteger = Val . fromInteger

instance Fractional a => Fractional (Val a) where
  a / b = Div a b
  fromRational = Val . fromRational

eval :: (GetVal n, Ord n, Fractional n) => Box h v n -> (Vector h n, Vector v n)
eval Box = (VNil, VNil)
eval (Merge bs) = (Vector start1' end1' middle1' size1', Vector start' end' middle' size')
  where
    ebs = evalV <$> bs
    start' = minimum $ getStart <$> ebs
    end' = maximum $ getEnd <$> ebs
    size' = end' - start'
    middle' = start' + size' / 2
    ebs1 = evalH <$> bs
    start1' = minimum $ getStart <$> ebs1
    end1' = maximum $ getEnd <$> ebs1
    size1' = end1' - start1'
    middle1' = start1' + size1' / 2
eval (Row _) = (VNil, VNil)
eval (Col _) = (VNil, VNil)
eval (SetWidth n b) = (size n $ evalH b, evalV b)
eval (SetLeft n b) = (start n $ evalH b, evalV b)
eval (SetRight n b) = (end n $ evalH b, evalV b)
eval (SetCenterX n b) = (middle n $ evalH b, evalV b)
eval (SetHeight n b) = (evalH b, size n $ evalV b)
eval (SetTop n b) = (evalH b, start n $ evalV b)
eval (SetBottom n b) = (evalH b, end n $ evalV b)
eval (SetCenterY n b) = (evalH b, middle n $ evalV b)
eval (Unwrap b) = (evalH (getVal <$> b), evalV (getVal <$> b))
eval (IfElse p a b) = if (getVal p) then (eval a) else (eval b)

evalH :: (GetVal n, Ord n, Fractional n) => Box h v n -> Vector h n
evalH = fst . eval

evalV :: (GetVal n, Ord n, Fractional n) => Box h v n -> Vector v n
evalV = snd . eval

box :: Box 'Empty 'Empty a
box = Box

merge :: [Box 'Constrained 'Constrained a] -> Box 'Constrained 'Constrained a
merge = Merge

row :: (CanStart h, HasSize h) => [Box h v a] -> Box 'Empty 'Empty a
row = Row

col :: (CanStart v, HasSize v) => [Box h v a] -> Box 'Empty 'Empty a
col = Col

width :: CanSize h => a -> Box h v a -> Box (AddSize h) v a
width = SetWidth

height :: CanSize v => a -> Box h v a -> Box h (AddSize v) a
height = SetHeight

top :: CanStart v => a -> Box h v a -> Box h (AddStart v) a
top = SetTop

bottom :: CanEnd v => a -> Box h v a -> Box h (AddEnd v) a
bottom = SetBottom

left :: CanStart h => a -> Box h v a -> Box (AddStart h) v a
left = SetLeft

right :: CanEnd h => a -> Box h v a -> Box (AddEnd h) v a
right = SetRight

centerX :: CanMiddle h => a -> Box h v a -> Box (AddMiddle h) v a
centerX = SetCenterX

centerY :: CanMiddle v => a -> Box h v a -> Box h (AddMiddle v) a
centerY = SetCenterY

class GetVal a where
  getVal :: Val a -> a

type family F a :: Bool where
  F Bool = 'True
  F a = 'False

instance (F a ~ flag, GetVal' flag a) => GetVal a where
  getVal = getVal' (Proxy :: Proxy flag)

class GetVal' (flag :: Bool) a where
  getVal' :: Proxy flag -> Val a -> a

instance GetVal' 'True Bool where
  getVal' _ (Val a) = a
  -- getVal' _ (Eq a b) = getVal a == getVal b
  -- getVal' _ (Gt a b) = getVal a > getVal b
  -- getVal' _ (Lt a b) = getVal a < getVal b
  -- getVal' _ (Not a) = not $ getVal a
  -- getVal' _ (Or a b) = getVal a || getVal b
  -- getVal' _ (And a b) = getVal a && getVal b
  getVal' _ a = getVal a

instance (GetVal' (F a) a, Fractional a, Ord a) => GetVal' 'False a where
  getVal' _ (Val a) = a
  getVal' _ (Plus a b) = getVal a + getVal b
  getVal' _ (Minus a b) = getVal a - getVal b
  getVal' _ (Times a b) = getVal a * getVal b
  getVal' _ (Div a b) = getVal a / getVal b
  getVal' _ (Abs a) = abs (getVal a)
  getVal' _ (Signum a) = signum (getVal a)
  getVal' _ (GetWidth b) = getSize (evalH b)
  getVal' _ (GetHeight b) = getSize (evalV b)
  getVal' _ (GetTop b) = getStart (evalV b)
  getVal' _ (GetBottom b) = getEnd (evalV b)
  getVal' _ (GetLeft b) = getStart (evalH b)
  getVal' _ (GetRight b) = getEnd (evalH b)
  getVal' _ (GetCenterX b) = getMiddle (evalH b)
  getVal' _ (GetCenterY b) = getMiddle (evalV b)

-- getVal' _ a = getVal a

class HasHeight f where
  getHeight :: f -> Val Double

instance HasSize v => HasHeight (Box h v (Val Double)) where
  getHeight = GetHeight . Unwrap

instance HasSize v => HasHeight (Box h v Double) where
  getHeight = GetHeight

class HasWidth f where
  getWidth :: f -> Val Double

instance HasSize h => HasWidth (Box h v (Val Double)) where
  getWidth = GetWidth . Unwrap

instance HasSize h => HasWidth (Box h v Double) where
  getWidth = GetWidth

class HasLeft f where
  getLeft :: f -> Val Double

instance HasStart h => HasLeft (Box h v (Val Double)) where
  getLeft = GetLeft . Unwrap

instance HasStart h => HasLeft (Box h v Double) where
  getLeft = GetLeft

class HasRight f where
  getRight :: f -> Val Double

instance HasEnd h => HasRight (Box h v (Val Double)) where
  getRight = GetRight . Unwrap

instance HasEnd h => HasRight (Box h v Double) where
  getRight = GetRight

class HasTop f where
  getTop :: f -> Val Double

instance HasStart v => HasTop (Box h v (Val Double)) where
  getTop = GetTop . Unwrap

instance HasStart v => HasTop (Box h v Double) where
  getTop = GetTop

class HasBottom f where
  getBottom :: f -> Val Double

instance HasEnd v => HasBottom (Box h v (Val Double)) where
  getBottom = GetBottom . Unwrap

instance HasEnd v => HasBottom (Box h v Double) where
  getBottom = GetBottom

class HasCenterX f where
  getCenterX :: f -> Val Double

instance HasMiddle h => HasCenterX (Box h v (Val Double)) where
  getCenterX = GetCenterX . Unwrap

instance HasMiddle h => HasCenterX (Box h v Double) where
  getCenterX = GetCenterX

class HasCenterY f where
  getCenterY :: f -> Val Double

instance HasMiddle v => HasCenterY (Box h v (Val Double)) where
  getCenterY = GetCenterY . Unwrap

instance HasMiddle v => HasCenterY (Box h v Double) where
  getCenterY = GetCenterY
