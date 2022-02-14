{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.Reader
import Data.Function ((&))
import Data.List (sort, sortOn)
import GHC.Float (int2Double)
import Text.Blaze
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html4.FrameSet.Attributes
  ( style,
  )
import qualified Text.Blaze.Html5 as Html

type View = Reader Box [Box]

data Box' h v = Box {horizontal :: Vector h, vertical :: Vector v} deriving (Show)

type Box = Box' Constrained Constrained

render :: IO ()
render = writeFile "rendered.html" $ renderHtml (toMarkup (runReader iphone (box & left 0 & top 0 & width 375 & height 812)))

printGrid :: IO ()
printGrid = print $ gridCSS $ toGrid iphone (box & left 0 & top 0 & width 375 & height 812)

gridCSS :: ([Double], [Double]) -> String
gridCSS (c, r) = colCSS <> rowCSS
  where
    addFR = foldMap ((<> "fr ") . show)
    colCSS = "grid-template-columns: " <> addFR c <> ";"
    rowCSS = "grid-template-rows: " <> addFR r <> ";"

toGrid :: View -> Box -> ([Double], [Double])
toGrid v container = (normalizedCols, normalizedRows)
  where
    rendered = runReader v container
    w = getWidth container
    h = getHeight container
    normalizedCols = (/ w) <$> getCols rendered
    normalizedRows = (/ h) <$> getRows rendered

getCols :: [Box' Constrained v] -> [Double]
getCols bs = zip sorted (tail sorted) >>= (\(a, b) -> if b - a > 0 then [b - a] else [])
  where
    sorted = sort $ horizontal <$> bs >>= \x -> [_start x, _end x]

getRows :: [Box' v Constrained] -> [Double]
getRows bs = zip sorted (tail sorted) >>= (\(a, b) -> if b - a > 0 then [b - a] else [])
  where
    sorted = sort $ vertical <$> bs >>= \x -> [_start x, _end x]

instance ToMarkup Box where
  toMarkup b = Html.div ! style ("position: absolute; background: rgba(0, 0, 0, 0.1); width: " <> w <> "; height: " <> h <> "; transform: translate(" <> p <> ");") $ mempty
    where
      w = stringValue $ show (getWidth b) <> "px"
      h = stringValue $ show (getHeight b) <> "px"
      l = show (getLeft b) <> "px"
      t = show (getTop b) <> "px"
      p = stringValue $ l <> ", " <> t

vw :: Fractional a => a -> a
vw px = (100 * px) / 1864

instance ToMarkup [Box] where
  toMarkup b = Html.div ! style "position: absolute; top: 0; left: 0; bottom: 0; right: 0;" $ Html.toHtml $ toMarkup <$> b

contains :: Box -> View -> View
contains c = local (const c)

app :: View
app = do
  container <- ask
  pure [box & width (getHeight container) & height (getHeight container) & left (getLeft container) & top (getTop container)]

appSize :: Double
appSize = 56

appRow :: View
appRow = do
  container <- ask
  icon <- (container & height appSize) `contains` app
  pure $ row 4 container (head icon)

iphone :: View
iphone = do
  screen <- ask
  appContainer <-
    pure $
      screen
        & width (getWidth screen - appSize)
        & centerX (getCenterX screen)
        & top (appSize / 1.618 * 2)
        & height (appSize * 6 * 1.618)
  bottomContainer <-
    pure $
      box
        & height appSize
        & width (getWidth screen - appSize)
        & centerX (getCenterX screen)
        & bottom (getHeight screen - (appSize / 1.618))
  r <- appContainer `contains` appRow
  b <- bottomContainer `contains` appRow
  apps <- pure $ col 6 appContainer =<< r
  pure $ screen : appContainer : bottomContainer : b ++ apps

row ::
  ( HasLeft container,
    HasWidth container,
    HasWidth (Box' h v),
    CanStart Vector h
  ) =>
  Int ->
  container ->
  Box' h v ->
  [Box' (AddStart h) v]
row n container b = boxes
  where
    bW = getWidth b
    cW = getWidth container
    cL = getLeft container

    boxes =
      [ left (cL + ((((cW - ((int2Double n) * bW)) / ((int2Double n) - 1)) + bW) * i)) x
        | (i, x) <- zip [0 ..] (replicate n b)
      ]

col ::
  ( HasTop container,
    HasHeight container,
    HasHeight (Box' h v),
    CanStart Vector v
  ) =>
  Int ->
  container ->
  Box' h v ->
  [Box' h (AddStart v)]
col n container b = boxes
  where
    bW = getHeight b
    cW = getHeight container
    cL = getTop container

    boxes =
      [ top (cL + ((((cW - ((int2Double n) * bW)) / ((int2Double n) - 1)) + bW) * i)) x
        | (i, x) <- zip [0 ..] (replicate n b)
      ]

box :: Box' '[] '[]
box = Box VNil VNil

top :: CanStart Vector v => Double -> Box' h v -> Box' h (AddStart v)
top n (Box h v) = Box h (start n v)

bottom :: CanEnd Vector v => Double -> Box' h v -> Box' h (AddEnd v)
bottom n (Box h v) = Box h (end n v)

left :: CanStart Vector h => Double -> Box' h v -> Box' (AddStart h) v
left n (Box h v) = Box (start n h) v

right :: CanEnd Vector h => Double -> Box' h v -> Box' (AddEnd h) v
right n (Box h v) = Box (end n h) v

centerY :: CanMiddle Vector v => Double -> Box' h v -> Box' h (AddMiddle v)
centerY n (Box h v) = Box h (middle n v)

centerX :: CanMiddle Vector h => Double -> Box' h v -> Box' (AddMiddle h) v
centerX n (Box h v) = Box (middle n h) v

width :: CanSize Vector h => Double -> Box' h v -> Box' (AddSize h) v
width n (Box h v) = Box (size n h) v

height :: CanSize Vector v => Double -> Box' h v -> Box' h (AddSize v)
height n (Box h v) = Box h (size n v)

data Vector (attrs :: [VAttrs]) where
  VNil :: Vector '[]
  VStart :: Double -> Vector '[ 'Start]
  VEnd :: Double -> Vector '[ 'End]
  VMiddle :: Double -> Vector '[ 'Middle]
  VSize :: Double -> Vector '[ 'Size]
  Vector ::
    { _start :: Double,
      _end :: Double,
      _middle :: Double,
      _size :: Double
    } ->
    Vector Constrained

deriving instance Show (Vector attrs)

data VAttrs = Start | End | Middle | Size

type Constrained = '[ 'Start, 'Middle, 'End, 'Size]

vector :: Vector '[]
vector = VNil

class CanMiddle f (as :: [VAttrs]) where
  type AddMiddle as :: [VAttrs]
  middle :: Double -> f as -> f (AddMiddle as)

instance CanMiddle Vector '[] where
  type AddMiddle '[] = '[ 'Middle]
  middle n _ = VMiddle n

instance CanMiddle Vector '[ 'Start] where
  type AddMiddle '[ 'Start] = Constrained
  middle n (VStart s) = Vector s e n m
    where
      m = (n - s) * 2
      e = s + m

instance CanMiddle Vector '[ 'End] where
  type AddMiddle '[ 'End] = Constrained
  middle n (VEnd e) = Vector s e n m
    where
      s = e - m
      m = (e - n) * 2

instance CanMiddle Vector '[ 'Size] where
  type AddMiddle '[ 'Size] = Constrained
  middle n (VSize m) = Vector s e n m
    where
      s = n - (m / 2)
      e = s + m

instance CanMiddle Vector '[ 'Middle] where
  type AddMiddle '[ 'Middle] = '[ 'Middle]
  middle n _ = VMiddle n

instance CanMiddle Vector Constrained where
  type AddMiddle Constrained = Constrained
  middle n (Vector _ _ _ l) = Vector (n - (l / 2)) (n + (l / 2)) n l

class CanStart f (as :: [VAttrs]) where
  type AddStart as :: [VAttrs]
  start :: Double -> f as -> f (AddStart as)

instance CanStart Vector '[] where
  type AddStart '[] = '[ 'Start]
  start n _ = VStart n

instance CanStart Vector '[ 'End] where
  type AddStart '[ 'End] = Constrained
  start n (VEnd e) = Vector n e c m
    where
      m = e - n
      c = m / 2

instance CanStart Vector '[ 'Size] where
  type AddStart '[ 'Size] = Constrained
  start n (VSize m) = Vector n e c m
    where
      e = n + m
      c = n + (m / 2)

instance CanStart Vector '[ 'Middle] where
  type AddStart '[ 'Middle] = Constrained
  start n (VMiddle c) = Vector n e c m
    where
      m = (c - n) * 2
      e = n + m

instance CanStart Vector '[ 'Start] where
  type AddStart '[ 'Start] = '[ 'Start]
  start n _ = VStart n

instance CanStart Vector Constrained where
  type AddStart Constrained = Constrained
  start n (Vector _ _ _ l) = Vector n (n + l) (n + (l / 2)) l

class CanEnd f (as :: [VAttrs]) where
  type AddEnd as :: [VAttrs]
  end :: Double -> f as -> f (AddEnd as)

instance CanEnd Vector '[] where
  type AddEnd '[] = '[ 'End]
  end n _ = VEnd n

instance CanEnd Vector '[ 'Start] where
  type AddEnd '[ 'Start] = Constrained
  end n (VStart s) = Vector s n c m
    where
      m = n - s
      c = s + (m / 2)

instance CanEnd Vector '[ 'Middle] where
  type AddEnd '[ 'Middle] = Constrained
  end n (VMiddle c) = Vector s n c m
    where
      m = (n - c) * 2
      s = n - m

instance CanEnd Vector '[ 'Size] where
  type AddEnd '[ 'Size] = Constrained
  end n (VSize m) = Vector s n c m
    where
      s = n - m
      c = s + (m / 2)

instance CanEnd Vector '[ 'End] where
  type AddEnd '[ 'End] = '[ 'End]
  end n _ = VEnd n

instance CanEnd Vector Constrained where
  type AddEnd Constrained = Constrained
  end n (Vector _ _ _ l) = Vector (n - l) n (n - (l / 2)) l

class CanSize f (as :: [VAttrs]) where
  type AddSize as :: [VAttrs]
  size :: Double -> f as -> f (AddSize as)

instance CanSize Vector '[] where
  type AddSize '[] = '[ 'Size]
  size n _ = VSize n

instance CanSize Vector '[ 'Start] where
  type AddSize '[ 'Start] = Constrained
  size n (VStart s) = Vector s e c n
    where
      e = s + n
      c = s + (n / 2)

instance CanSize Vector '[ 'End] where
  type AddSize '[ 'End] = Constrained
  size n (VEnd e) = Vector s e c n
    where
      s = e - n
      c = s + (n / 2)

instance CanSize Vector '[ 'Middle] where
  type AddSize '[ 'Middle] = Constrained
  size n (VMiddle c) = Vector s e c n
    where
      e = c + (n / 2)
      s = c - (n / 2)

instance CanSize Vector '[ 'Size] where
  type AddSize '[ 'Size] = '[ 'Size]
  size n _ = VSize n

instance CanSize Vector Constrained where
  type AddSize Constrained = Constrained
  size n (Vector s _ _ _) = Vector s (s + n) (s + (n / 2)) n

class HasStart f (as :: [VAttrs]) where
  getStart :: f as -> Double

instance HasStart Vector '[ 'Start] where
  getStart (VStart n) = n

instance HasStart Vector Constrained where
  getStart = _start

class HasMiddle f (as :: [VAttrs]) where
  getMiddle :: f as -> Double

instance HasMiddle Vector '[ 'Middle] where
  getMiddle (VMiddle n) = n

instance HasMiddle Vector Constrained where
  getMiddle = _middle

class HasEnd f (as :: [VAttrs]) where
  getEnd :: f as -> Double

instance HasEnd Vector '[ 'End] where
  getEnd (VEnd n) = n

instance HasEnd Vector Constrained where
  getEnd = _end

class HasSize f (as :: [VAttrs]) where
  getSize :: f as -> Double

instance HasSize Vector '[ 'Size] where
  getSize (VSize n) = n

instance HasSize Vector Constrained where
  getSize = _size

class HasWidth f where
  getWidth :: f -> Double

instance HasSize Vector h => HasWidth (Box' h v) where
  getWidth = getSize . horizontal

class HasHeight f where
  getHeight :: f -> Double

instance HasSize Vector v => HasHeight (Box' h v) where
  getHeight = getSize . vertical

class HasTop f where
  getTop :: f -> Double

instance HasStart Vector v => HasTop (Box' h v) where
  getTop = getStart . vertical

class HasBottom f where
  getBottom :: f -> Double

instance HasEnd Vector v => HasBottom (Box' h v) where
  getBottom = getEnd . vertical

class HasLeft f where
  getLeft :: f -> Double

instance HasStart Vector h => HasLeft (Box' h v) where
  getLeft = getStart . horizontal

class HasRight f where
  getRight :: f -> Double

instance HasEnd Vector h => HasRight (Box' h v) where
  getRight = getEnd . horizontal

class HasCenterX f where
  getCenterX :: f -> Double

instance HasMiddle Vector h => HasCenterX (Box' h v) where
  getCenterX = getMiddle . horizontal

class HasCenterY f where
  getCenterY :: f -> Double

instance HasMiddle Vector v => HasCenterY (Box' h v) where
  getCenterY = getMiddle . vertical
