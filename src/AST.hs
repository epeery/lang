{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module AST where

import Control.Category
import ITree
import Positionable
import Prelude hiding (Left, Right, id, (.))

type AST = ITree (Layout Cat)

data Cat a b where
  Id :: Cat a a
  Dot :: Cat b c -> Cat a b -> Cat a c
  Val :: Double -> Cat f Double
  Box :: Cat f (Empty, Empty)
  Container :: Cat Ctx Positioned
  Screen :: Cat Ctx Positioned
  Width :: CanSize h => (Cat f Double) -> (Cat f (h, v)) -> Cat f (Add Size h, v)
  GetWidth :: HasSize h => (Cat f (h, v)) -> Cat f Double
  Height :: CanSize v => (Cat f Double) -> (Cat f (h, v)) -> Cat f (h, Add Size v)
  GetHeight :: HasSize v => (Cat f (h, v)) -> Cat f Double
  Left :: CanStart h => (Cat f Double) -> (Cat f (h, v)) -> Cat f (Add Start h, v)
  GetLeft :: HasStart h => (Cat f (h, v)) -> Cat f Double
  Right :: CanEnd h => (Cat f Double) -> (Cat f (h, v)) -> Cat f (Add End h, v)
  GetRight :: HasEnd h => (Cat f (h, v)) -> Cat f Double
  Top :: CanStart v => (Cat f Double) -> (Cat f (h, v)) -> Cat f (h, Add Start v)
  GetTop :: HasStart v => (Cat f (h, v)) -> Cat f Double
  Bottom :: CanEnd v => (Cat f Double) -> (Cat f (h, v)) -> Cat f (h, Add End v)
  GetBottom :: HasEnd v => (Cat f (h, v)) -> Cat f Double
  CenterX :: CanMiddle h => (Cat f Double) -> (Cat f (h, v)) -> Cat f (Add Middle h, v)
  GetCenterX :: HasMiddle h => (Cat f (h, v)) -> Cat f Double
  CenterY :: CanMiddle v => (Cat f Double) -> (Cat f (h, v)) -> Cat f (h, Add Middle v)
  GetCenterY :: HasMiddle v => (Cat f (h, v)) -> Cat f Double
  Plus :: Cat f Double -> Cat f Double -> Cat f Double
  Minus :: Cat f Double -> Cat f Double -> Cat f Double
  Times :: Cat f Double -> Cat f Double -> Cat f Double
  Div :: Cat f Double -> Cat f Double -> Cat f Double
  Abs :: Cat f Double -> Cat f Double
  Signum :: Cat f Double -> Cat f Double

finalize ::
  ( Positionable repr,
    Containable repr,
    Category repr
  ) =>
  Cat a b ->
  repr a b
finalize Id = id
finalize (Dot b a) = (finalize b) . (finalize a)
finalize (Val d) = val d
finalize (Plus a b) = (finalize a) + (finalize b)
finalize (Minus a b) = (finalize a) - (finalize b)
finalize (Times a b) = (finalize a) * (finalize b)
finalize (Div a b) = (finalize a) / (finalize b)
finalize (Abs a) = abs (finalize a)
finalize (Signum a) = signum (finalize a)
finalize Box = box
finalize Container = container
finalize Screen = screen
finalize (Width a b) = width (finalize a) (finalize b)
finalize (GetWidth a) = getWidth (finalize a)
finalize (Height a b) = height (finalize a) (finalize b)
finalize (GetHeight a) = getHeight (finalize a)
finalize (Left a b) = left (finalize a) (finalize b)
finalize (GetLeft a) = getLeft (finalize a)
finalize (Right a b) = right (finalize a) (finalize b)
finalize (GetRight a) = getRight (finalize a)
finalize (Top a b) = top (finalize a) (finalize b)
finalize (GetTop a) = getTop (finalize a)
finalize (Bottom a b) = bottom (finalize a) (finalize b)
finalize (GetBottom a) = getBottom (finalize a)
finalize (CenterX a b) = centerX (finalize a) (finalize b)
finalize (GetCenterX a) = getCenterX (finalize a)
finalize (CenterY a b) = centerY (finalize a) (finalize b)
finalize (GetCenterY a) = getCenterY (finalize a)

instance (Num (Cat f Double)) where
  (+) = Plus
  (-) = Minus
  (*) = Times
  abs = Abs
  signum = Signum
  fromInteger n = Val $ fromInteger n

instance (Fractional (Cat f Double)) where
  (/) = Div
  fromRational n = Val $ fromRational n

deriving instance Show (Cat a b)

instance Category Cat where
  id = Id
  (.) = Dot

instance Semigroup (Cat a a) where
  (<>) = Dot

instance Monoid (Cat a a) where
  mempty = Id

instance Positionable Cat where
  val = Val
  box = Box
  left = Left
  getLeft = GetLeft
  right = Right
  getRight = GetRight
  width = Width
  getWidth = GetWidth
  centerX = CenterX
  getCenterX = GetCenterX
  top = Top
  getTop = GetTop
  bottom = Bottom
  getBottom = GetBottom
  height = Height
  getHeight = GetHeight
  centerY = CenterY
  getCenterY = GetCenterY

instance Containable Cat where
  container = Container
  screen = Screen
