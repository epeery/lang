{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Render where

import AST (AST)
import Box
import Control.Comonad.Cofree
import Data.Function ((&))
import Data.Functor.Compose
import Data.Tree (flatten)
import qualified Data.Tree as Tree
import ITree
import Lib
import Positionable
import Text.Blaze
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html4.FrameSet.Attributes
  ( style,
  )
import qualified Text.Blaze.Html5 as Html

type Width = Double

type Height = Double

-- render :: (forall c. (Containable c, Positionable c) => Dom (Layout c) (Layout c)) -> (AST, View)
-- render x = (view x, eval 1920 1080 x)

view :: AST -> AST
view = id

type View = Tree.Tree Positioned

eval :: Width -> Height -> Container -> View
eval w h = go s
  where
    s = (unBox $ box & width (pure w) & height (pure h) & top 0 & left 0) ()
    go :: Positioned -> Container -> View
    go a (Node (Box r) f as) = let c = (r (s, a)) in Tree.Node c ((go c . (f <$>)) <$> as)

-- eval' :: Width -> Height -> Cofree (Compose A []) (Layout Box) -> View
-- eval' w h = go s
--   where
--     s = (unBox $ box & width (pure w) & height (pure h) & top 0 & left 0) ()
--     go :: Positioned -> Cofree (Compose A []) (Layout Box) -> View
--     go a ((Box r) :< (Compose (A f as))) = let c = (r (s, a)) in Tree.Node c (go c <$> (f as))

instance ToMarkup (Constrained, Constrained) where
  toMarkup (Constrained (Start _left) _ _ (Size _width), Constrained (Start _top) _ _ (Size _height)) = Html.div ! style ("position: absolute; background: rgba(0, 0, 0, 0.1); width: " <> w <> "; height: " <> h <> "; transform: translate(" <> p <> ");") $ mempty
    where
      w = stringValue $ show (vw _width) <> "vw"
      h = stringValue $ show (vw _height) <> "vw"
      l = show (vw _left) <> "vw"
      t = show (vw _top) <> "vw"
      p = stringValue $ l <> ", " <> t

vw :: Fractional a => a -> a
vw px = (100 * px) / 1674

instance ToMarkup [(Constrained, Constrained)] where
  toMarkup b = Html.div ! style "position: absolute; top: 0; left: 0; bottom: 0; right: 0;" $ Html.toHtml $ toMarkup <$> b

-- render :: IO ()
render b = writeFile "rendered.html" $ renderHtml $ toMarkup b
