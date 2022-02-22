{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Propagator
import Data.Propagator.Cell
import GHC.Float (int2Double)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Box a = Box
  { _width :: a,
    _height :: a,
    _top :: a,
    _bottom :: a,
    _left :: a,
    _right :: a,
    _centerX :: a,
    _centerY :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

box :: ST s (Box (Cell s Double))
box = do
  w <- cell
  h <- cell
  t <- cell
  l <- cell
  b <- cell
  r <- cell
  cx <- cell
  cy <- cell

  lift2 (\n m -> m - n) l r w
  lift2 (\n m -> m + n) w l r
  lift2 (\n m -> m - n) w r l
  lift2 (\n m -> n + m / 2) l w cx
  lift2 (\n m -> n - m / 2) cx w l
  lift2 (\n m -> (m - n) * 2) l cx w
  lift2 (\n m -> (n - m) * 2) r cx w

  lift2 (\n m -> m - n) t b h
  lift2 (\n m -> m + n) h t b
  lift2 (\n m -> m - n) h b t
  lift2 (\n m -> n + m / 2) t h cy
  lift2 (\n m -> n - m / 2) cy h t
  lift2 (\n m -> (m - n) * 2) t cy h
  lift2 (\n m -> (n - m) * 2) b cy h

  return $ Box w h t b l r cx cy

constrain :: (Traversable t, Applicative f) => a -> t (a -> f b) -> f ()
constrain a b = traverse_ ($ a) b

same :: (Box (Cell s Double) -> Cell s Double) -> [Box (Cell s Double)] -> ST s ()
same f bs = fold $ zipWith (\a b -> unify (f a) (f b)) bs (tail bs)

combine :: [Box (Cell s Double)] -> ST s [Box (Maybe Double)]
combine = traverse (traverse content)

writeBox :: (Box (Cell s Double) -> Cell s Double) -> Double -> Box (Cell s Double) -> ST s ()
writeBox f n b = write (f b) n

width :: Double -> Box (Cell s Double) -> ST s ()
width = writeBox _width

height :: Double -> Box (Cell s Double) -> ST s ()
height = writeBox _height

top :: Double -> Box (Cell s Double) -> ST s ()
top = writeBox _top

left :: Double -> Box (Cell s Double) -> ST s ()
left = writeBox _left

bottom :: Double -> Box (Cell s Double) -> ST s ()
bottom = writeBox _bottom

right :: Double -> Box (Cell s Double) -> ST s ()
right = writeBox _right

centerX :: Double -> Box (Cell s Double) -> ST s ()
centerX = writeBox _centerX

centerY :: Double -> Box (Cell s Double) -> ST s ()
centerY = writeBox _centerY

center :: (Double, Double) -> Box (Cell s Double) -> ST s ()
center (x, y) b = centerX x b >> centerY y b

group :: [Box (Cell s Double)] -> ST s (Box (Cell s Double))
group = undefined

row :: Int -> ST s (Box (Cell s Double)) -> ST s [Box (Cell s Double)]
row n b = do
  container <- box

  b1 <- b
  boxes <- replicateM n b

  same _height [container, b1]

  watch2
    (_left container)
    (_width container)
    ( \containerLeft containerWidth ->
        with
          (_width b1)
          ( \bWidth ->
              for_
                (zip [0 ..] boxes)
                ( \(x, b') ->
                    left (containerLeft + ((((containerWidth - ((int2Double n) * bWidth)) / ((int2Double n) - 1)) + bWidth) * x)) b'
                )
          )
    )

  pure $ (container : boxes)

runBox :: [Box (Maybe Double)]
runBox = runST $ do
  screen <- box
  constrain screen [top 0, left 0, width 1640, height 893]

  a <- box
  b <- box
  c <- box

  same _left [a, screen]
  same _centerY [a, screen]
  with (_width screen) (\screenWidth -> constrain a [width (screenWidth / 5), height (screenWidth / 5)])

  same _width [a, b, c]
  same _height [a, b, c]
  same _top [a, b, c]

  same _right [c, screen]

  watch2 (_right a) (_left c) (\aRight bLeft -> centerX ((aRight + bLeft) / 2) b)

  combine [a, b, c]

iphone :: [Box (Maybe Double)]
iphone = runST $ do
  screen <- box
  constrain screen [left 0, top 0, width 375, height 812]

  Just screenWidth <- content (_width screen)

  (appRowContainer : appRow) <- row 4 makeApp
  constrain appRowContainer [left 28, right (screenWidth - 28)]
  combine appRow

makeApp :: ST s (Box (Cell s Double))
makeApp = do
  appIcon <- box
  constrain appIcon [width 58, height 58]
  return appIcon

test :: [Box (Maybe Double)]
test = runST $ do
  a <- box
  b <- box
  c <- box

  width 3 a
  height 1 a
  top 0 a
  left 0 a
  unify (_right a) (_left b)

  width 3 b
  height 4 b
  unify (_top b) (_top a)

  width 2 c
  height 2 c
  unify (_right c) (_left b)
  unify (_bottom c) (_bottom b)

  combine [a, b, c]
