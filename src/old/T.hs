{-# LANGUAGE OverloadedStrings #-}

module T where

import A
import Data.Function ((&))
import Data.Tree

t :: Tree (Box (Constrained, Constrained))
t = do
  screen <- pure $ box & left 0 & top 0 & width 375 & height 812
  something <- pure $ box & left 0 & top (getCenterY screen) & width (20) & height 20
  Node screen [Node something []]
