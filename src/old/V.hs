{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module V where

class V repr where
  val :: Double -> repr Double
  nil :: repr Empty

instance V ((->) (repr v)) where
  val = const
  nil = const Empty

-- instance Functor f => CanStart f Empty where
--   start fs _ = Start <$> fs

-- instance Applicative f => CanStart f Size where
--   start fs fl = (\s (Size l) -> Constrained s (s + l)) <$> fs <*> fl

-- instance Functor f => CanSize f Empty where
--   size fs _ = Size <$> fs

-- instance (V f, Functor f) => HasSize f Size where
--   getSize f = (\(Size l) -> l) <$> f

-- instance (V f, Functor f) => HasSize f Constrained where
--   getSize f = (\(Constrained s e) -> e - s) <$> f

-- instance (V f, Functor f) => HasStart f Start where
--   getStart f = (\(Start s) -> s) <$> f

-- instance (V f, Functor f) => HasStart f Constrained where
--   getStart f = (\(Constrained s _) -> s) <$> f

-- instance Applicative f => CanEnd f Start where
--   end fe fs = (\(Start s) e -> Constrained s e) <$> fs <*> fe

-- instance CanStart ((->) (repr v)) Empty where
--   start r2d _ r = Start (r2d r)

-- instance (V repr, CanStart repr v) => CanStart ((->) (repr v)) (repr v) where
--   start r2d r2v r =
--     let v = r2v r
--         d = r2d r
--      in start (val d) v

class CanStart repr v where
  start :: repr Double -> repr v -> repr (Add Start v)

class V repr => HasStart repr v where
  getStart :: repr v -> repr Double

class CanEnd repr v where
  end :: repr Double -> repr v -> repr (Add End v)

class V repr => HasEnd repr v where
  getEnd :: repr v -> repr Double

class CanMiddle repr v where
  middle :: repr Double -> repr v -> repr (Add Middle v)

class V repr => HasMiddle repr v where
  getMiddle :: repr v -> repr Double

class CanSize repr v where
  size :: repr Double -> repr v -> repr (Add Size v)

class V repr => HasSize repr v where
  getSize :: repr v -> repr Double

data Empty = Empty deriving (Show)

data Start = Start Double deriving (Show)

data Middle deriving (Show)

data End = End Double deriving (Show)

data Size = Size Double deriving (Show)

data Constrained = Constrained Double Double deriving (Show)

type family Add a b where
  Add a Empty = a
  Add Empty b = b
  Add a a = a
  Add (f a) (f b) = f (Add a b)
  Add (f a) b = f (Add a b)
  Add a (f b) = f (Add a b)
  Add a b = Constrained

data Vector a where
  Val :: Double -> Vector Double
  VNil :: Vector Empty
  VStart :: Double -> Vector Start
  VEnd :: Double -> Vector End
  VMiddle :: Double -> Vector Middle
  VSize :: Double -> Vector Size
  Vector :: {_start :: Double, _middle :: Double, _end :: Double, _size :: Double} -> Vector Constrained

deriving instance Show a => Show (Vector a)

-- instance Show a => Show (Vector a) where
--   show (Val a) = show a
--   show (VNil) = "Nil"
--   show (VStart n) = "Start " <> show n
--   show (VEnd n) = "End " <> show n
--   show (VMiddle n) = "Middle " <> show n
--   show (VSize n) = "Size " <> show n
--   show (Vector s m e l) = "Vector " <> show s <> " " <> show m <> " " <> show e <> " " <> " " <> show l

instance Num (Vector Double) where
  (Val a) + (Val b) = Val $ a + b
  (Val a) - (Val b) = Val $ a - b
  (Val a) * (Val b) = Val $ a * b
  abs (Val a) = Val $ abs a
  signum (Val a) = Val (signum a)
  fromInteger = Val . fromInteger

instance Fractional (Vector Double) where
  (Val a) / (Val b) = Val $ a / b
  fromRational = Val . fromRational

instance V Vector where
  val = Val
  nil = VNil

instance CanStart Vector Empty where
  start (Val n) _ = VStart n

instance CanStart Vector Start where
  start (Val n) _ = VStart n

instance CanStart Vector End where
  start (Val n) (VEnd e) = Vector n ((n + e) / 2) e (e - n)

instance CanStart Vector Middle where
  start (Val n) (VMiddle m) = Vector n m e (e - n)
    where
      e = m + (m - n)

instance CanStart Vector Size where
  start (Val n) (VSize l) = Vector n (n + l / 2) (n + l) l

instance CanStart Vector Constrained where
  start (Val n) (Vector _ _ _ l) = Vector n (n + l / 2) (n + l) l

instance HasStart Vector Start where
  getStart (VStart n) = val n

instance HasStart Vector Constrained where
  getStart = val . _start

instance CanEnd Vector Empty where
  end (Val n) _ = VEnd n

instance CanEnd Vector End where
  end (Val n) _ = VEnd n

instance CanEnd Vector Start where
  end (Val n) (VStart s) = Vector s ((s + n) / 2) n (n - s)

instance CanEnd Vector Middle where
  end (Val n) (VMiddle m) = Vector s m n (n - s)
    where
      s = m - (n - m)

instance CanEnd Vector Size where
  end (Val n) (VSize l) = Vector (n - l) (n - l / 2) n l

instance CanEnd Vector Constrained where
  end (Val n) (Vector _ _ _ l) = Vector (n - l) (n - l / 2) n l

instance HasEnd Vector End where
  getEnd (VEnd n) = val n

instance HasEnd Vector Constrained where
  getEnd = val . _end

instance CanMiddle Vector Empty where
  middle (Val n) _ = VMiddle n

instance CanMiddle Vector Middle where
  middle (Val n) _ = VMiddle n

instance CanMiddle Vector Start where
  middle (Val n) (VStart s) = Vector s n e (e - s)
    where
      e = n + (n - s)

instance CanMiddle Vector End where
  middle (Val n) (VEnd e) = Vector s n e (e - s)
    where
      s = n - (e - n)

instance CanMiddle Vector Size where
  middle (Val n) (VSize l) = Vector (n - l / 2) n (n + l / 2) l

instance CanMiddle Vector Constrained where
  middle (Val n) (Vector _ _ _ l) = Vector (n - l / 2) n (n + l / 2) l

instance HasMiddle Vector Middle where
  getMiddle (VMiddle n) = val n

instance HasMiddle Vector Constrained where
  getMiddle = val . _middle

instance CanSize Vector Empty where
  size (Val n) _ = VSize n

instance CanSize Vector Size where
  size (Val n) _ = VSize n

instance CanSize Vector Start where
  size (Val n) (VStart s) = Vector s (s + n / 2) (s + n) n

instance CanSize Vector End where
  size (Val n) (VEnd e) = Vector (e - n) (e - n / 2) e n

instance CanSize Vector Middle where
  size (Val n) (VMiddle m) = Vector (m - n / 2) m (m + n / 2) n

instance CanSize Vector Constrained where
  size (Val n) (Vector _ m _ _) = Vector (m - n / 2) m (m + n / 2) n

instance HasSize Vector Size where
  getSize (VSize n) = val n

instance HasSize Vector Constrained where
  getSize = val . _size

instance V S where
  val n = S $ show n
  nil = S "Nil"

space :: (Fractional (repr Double), HasSize repr v, CanStart repr v) => repr Double -> repr Double -> [repr v] -> [repr (Add Start v)]
space margin begin cs = snd $ foldr (\v (offset, acc) -> (offset + margin + getSize v, start offset v : acc)) (begin, []) cs

spaceBetween :: (Fractional (repr Double), HasSize repr c, HasSize repr v, CanStart repr v) => repr c -> [repr v] -> [repr (Add Start v)]
spaceBetween c cs = space margin (val 0) cs
  where
    contentSize = sum (getSize <$> cs)
    margin = (getSize c - contentSize) / (val $ toEnum $ length cs - 1)

spaceAround :: (Fractional (repr Double), HasSize repr c, HasSize repr v, CanStart repr v) => repr c -> [repr v] -> [repr (Add Start v)]
spaceAround c cs = space margin margin cs
  where
    contentSize = sum (getSize <$> cs)
    margin = (getSize c - contentSize) / (val $ toEnum $ length cs + 1)

-- test ::
--   ( Fractional (repr Double),
--     HasSize repr Constrained,
--     HasSize repr Size,
--     CanStart repr Size,
--     CanStart repr Empty,
--     CanSize repr Start,
--     CanSize repr Empty
--   ) =>
--   [repr Constrained]
-- test = (size 10 $ start 0 $ nil) `spaceAround` (replicate 3 (size 2 nil))

newtype S a = S {unS :: String} deriving (Show, Functor)

instance Applicative S where
  pure _ = S ""
  S f <*> S a = S $ f <> " " <> a

instance Num (S a) where
  S a + S b = S $ a <> " + " <> b
  S a - S b = S $ a <> " - " <> b
  S a * S b = S $ a <> " * " <> b
  abs (S a) = S $ "abs (" <> a <> ")"
  signum (S a) = S $ "signum (" <> a <> ")"
  fromInteger = S . show

instance Fractional (S a) where
  (S a) / (S b) = S $ a <> " / " <> b
  fromRational = S . show

-- instance CanStart S v where
--   start (S n) (S v) = S $ "SetStart " <> n <> " (" <> v <> ")"

instance HasStart S v where
  getStart (S n) = S $ "GetStart (" <> n <> ")"

instance CanEnd S v where
  end (S n) (S v) = S $ "SetEnd " <> n <> " (" <> v <> ")"

instance HasEnd S v where
  getEnd (S n) = S $ "GetEnd (" <> n <> ")"

instance CanMiddle S v where
  middle (S n) (S v) = S $ "SetMiddle " <> n <> " (" <> v <> ")"

instance HasMiddle S v where
  getMiddle (S n) = S $ "GetMiddle (" <> n <> ")"

instance CanSize S v where
  size (S n) (S v) = S $ "SetSize " <> n <> " (" <> v <> ")"

instance HasSize S v where
  getSize (S n) = S $ "GetSize (" <> n <> ")"
