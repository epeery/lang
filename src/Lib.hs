{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib where

import Control.Comonad.Cofree
import Data.Bitraversable (bisequenceA)
import Data.Function ((&))
import Data.Functor.Compose
import ITree
import Positionable

appSize :: Num p => p
appSize = 58

app' :: (Positionable f, Num (f r Double)) => ITree (f r (Size, Size))
app' = Node (box & width appSize & height appSize) id []

appRow' ::
  (Fractional (f Ctx Double), Positionable f, Containable f, Applicative (f Ctx)) =>
  ITree (f Ctx (Constrained, Constrained))
appRow' = col' $ replicate 6 (row' (replicate 4 app'))

app :: (Positionable f, Containable f) => ITree (Layout f)
app = Node (box & width appSize & left (getLeft container) & fullHeight) id []

appRow :: (Positionable f, Containable f) => ITree (Layout f)
appRow =
  row
    (box & fullWidth & height appSize & top (getTop container))
    (replicate 4 app)

apps :: (Positionable c, Containable c) => ITree (Layout c)
apps =
  col
    (box & fullWidth & fullHeight)
    (replicate 5 appRow)

appContainer :: (Positionable f, Containable f) => Layout f
appContainer =
  box
    & width (getWidth container - appSize)
    & centerX (getCenterX container)
    & top (appSize / 1.618 * 2)
    & height (appSize * 6 * 1.618)

bottomContainer :: (Positionable f, Containable f) => Layout f
bottomContainer =
  box
    & height appSize
    & width (getWidth screen - appSize)
    & centerX (getCenterX screen)
    & bottom (getHeight screen - (appSize / 1.618))

iphone :: (Positionable c, Containable c) => ITree (Layout c)
iphone =
  Node
    (box & fullWidth & fullHeight)
    id
    [ Node appContainer id [apps],
      Node bottomContainer id [appRow]
    ]

fullWidth ::
  ( Positionable f,
    CanSize (Add Start h),
    Containable f,
    CanStart h
  ) =>
  f Ctx (h, v) ->
  f Ctx (Add Size (Add Start h), v)
fullWidth = width (getWidth container) . left (getLeft container)

fullHeight ::
  ( Positionable f,
    CanSize (Add Start v),
    Containable f,
    CanStart v
  ) =>
  f Ctx (h, v) ->
  f Ctx (h, Add Size (Add Start v))
fullHeight = height (getHeight container) . top (getTop container)

row' as = Node b (\x -> width (getWidth x) x) (foldl f [] as)
  where
    b = width (getWidth container) $ left (getLeft container) $ bisequenceA $ bounds $ sequenceA ((\(Node a _ _) -> a) <$> as)
    contentSize = sum $ (\(Node a _ _) -> getWidth a) <$> as
    margin = (getWidth container - contentSize) / (fromIntegral $ length as - 1)
    f = \acc e -> case acc of
      [] -> (left (getLeft container) <$> e) : acc
      (Node prev _ _ : _) -> (left (getRight prev + margin) <$> e) : acc

col' as = Node b (\x -> height (getHeight x) x) (foldl f [] as)
  where
    b = height (getHeight container) $ top (getTop container) $ bisequenceA $ bounds $ sequenceA ((\(Node a _ _) -> a) <$> as)
    contentSize = sum $ (\(Node a _ _) -> getHeight a) <$> as
    margin = (getHeight container - contentSize) / (fromIntegral $ length as - 1)
    f = \acc e -> case acc of
      [] -> (top (getTop container) <$> e) : acc
      (Node prev _ _ : _) -> (top (getBottom prev + margin) <$> e) : acc

row c as = Node c id (foldl f [] as)
  where
    contentSize = sum $ (\(Node a _ _) -> getWidth a) <$> as
    margin = (getWidth container - contentSize) / (fromIntegral $ length as - 1)
    f = \acc e -> case acc of
      [] -> (left (getLeft container) <$> e) : acc
      (Node prev _ _ : _) -> (left (getRight prev + margin) <$> e) : acc

col c as = Node c id (foldl f [] as)
  where
    contentSize = sum $ (\(Node a _ _) -> getHeight a) <$> as
    margin = (getHeight container - contentSize) / (fromIntegral $ length as - 1)
    f = \acc e -> case acc of
      [] -> (top (getTop container) <$> e) : acc
      (Node prev _ _ : _) -> (top (getBottom prev + margin) <$> e) : acc
