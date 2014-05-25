{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Map.KDMap where

import Control.Applicative
import Data.Maybe (maybeToList)
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Foldable as F

data KDMap k a = KDEmpty
               | KDLeaf   k a Depth
               | KDBranch k a Depth (KDMap k a) (KDMap k a)
               deriving (Eq, Show)

newtype Weight = Weight {unWeight :: Double}
                 deriving (Show, Eq, Num, Enum, Fractional)

newtype Depth = Depth {unDepth :: Int}
              deriving (Eq,Show,Num,Enum,Integral,Real,Ord)

class KDKey k where
  pointD       :: k -> Depth -> Double
  pointW       :: k -> Weight
  pointSize    :: k -> Depth

  pointDDistSq :: k -> k -> Depth -> Double
  pointDDistSq a b i = (pointD b i - pointD a i)^(2::Int)

  pointDistSq  :: k -> k -> Double
  pointDistSq a b = sum $ map (pointDDistSq a b)
                    [0..pointSize a - 1]

  dimOrder :: k -> k -> Depth -> Ordering
  dimOrder a b n = compare (pointD a n) (pointD b n)

  dSucc :: k -> Depth -> Depth
  dPred :: k -> Depth -> Depth

instance F.Foldable (KDMap k) where
  foldr _ z KDEmpty = z
  foldr f z (KDLeaf _ a _) = f a z
  foldr f z (KDBranch _ v _ kdLeft kdRight) = F.foldr f (f v (F.foldr f z kdLeft)) kdRight

data Point2 = Point2 {p2x :: Double
                     ,p2y :: Double
                     ,p2w :: Weight
                     }
                 deriving (Show,Eq)

instance KDKey Point2 where
  pointD p 0 = p2x p
  pointD p 1 = p2y p
  pointD _              n = error $ "Point2 out of bounds index: " ++ show n
  pointSize _    = 2
  pointW p   = p2w p
  dSucc p d = succ d `mod` fromIntegral (pointSize p)
  dPred p d = pred d `mod` fromIntegral (pointSize p)

toList :: KDMap k a -> [(k,a)]
toList KDEmpty = []
toList (KDLeaf k a _) = [(k,a)]
toList (KDBranch k a _ kdLeft kdRight) = (k,a) : toList kdLeft ++ toList kdRight

closer :: (Eq k, KDKey k) => Maybe (k,a) -> Maybe (k,a) -> k -> Maybe (k,a)
closer Nothing Nothing _ = Nothing
closer a Nothing _ = a
closer Nothing b _ = b
closer (Just optA@(kA,_)) (Just optB@(kB,_)) k
  | pointDistSq kA k < pointDistSq kB k = Just optA
  | otherwise                           = Just optB

delete :: (Show k, Eq k, KDKey k) => k -> KDMap k a -> KDMap k a
delete _ KDEmpty = KDEmpty
delete k m@(KDLeaf k' _ _)
  | k == k'   = KDEmpty
--  | otherwise = KDEmpty -- TODO really?
  | otherwise = trace ("Delete leaf. k:" ++ show k ++ " k':" ++ show k' ++ "\n") m
delete k (KDBranch k' a' d kdLeft kdRight)
  | k == k' = fromListWithDepth d (toList kdLeft ++ toList kdRight)
  | otherwise = trace ("delete Branch") $ case dimOrder k k' d of
    EQ -> KDBranch k' a' d (delete k kdLeft) (delete k kdRight)
    LT -> KDBranch k' a' d (delete k kdLeft)  kdRight
    GT -> KDBranch k' a' d  kdLeft           (delete k kdRight)


delete' :: (Show k, Eq k, KDKey k, Eq a) => k -> KDMap k a -> KDMap k a
delete' _ KDEmpty = KDEmpty
delete' k l@(KDLeaf k' _ _)
  | k == k' = KDEmpty
  | otherwise = l
delete' k (KDBranch k' a' d' kdLeft kdRight)
  | k == k' = fromListWithDepth d' (toList kdLeft ++ toList kdRight)
  | otherwise = KDBranch k' a' d' (subTree kdLeft) (subTree kdRight)
  where
    subTree t = if   (fst <$> closest k t) == Just k
                then delete k t
                else t
                

closest :: (Eq a, Eq k, KDKey k) => k -> KDMap k a -> Maybe (k,a)
closest _ KDEmpty = Nothing
closest _ (KDLeaf k' a' _) = Just (k',a')
closest k (KDBranch k' a' d' kdLeft kdRight) = case dimOrder k k' d' of
  LT -> findNearest kdLeft  kdRight
  _  -> findNearest kdRight kdLeft
  where
    findNearest treeA treeB =
      let mainCandidates = case closest k treeA of
            Nothing  -> [(k',a')]
            Just (k'',a'') -> [(k',a'),(k'',a'')]
          otherCandidates
            | (pointDistSq k k') >= (pointD k d' - pointD k' d')^(2::Int) =
              maybeToList (closest k treeB)
            | otherwise = []
      in Just $ L.minimumBy (comparing (pointDistSq k . fst))
         (mainCandidates ++ otherCandidates)

isValid :: (Eq k, KDKey k,Show a,Show k) => KDMap k a -> Bool
isValid KDEmpty = True
isValid (KDLeaf _ _ _) = True
isValid (KDBranch k _ d kdLeft kdRight) = thisValid && isValid kdLeft && isValid kdRight
  where thisValid = all (\(k',_) -> dimOrder k' k d == LT) (toList kdLeft)
                    &&
                    all (\(k',_) -> dimOrder k' k d /= LT) (toList kdRight)

fromListWithDepth :: (KDKey k) => Depth -> [(k,a)] -> KDMap k a
fromListWithDepth _ [] = KDEmpty
fromListWithDepth d [(k,a)] = KDLeaf k a d
fromListWithDepth d ps@((k,_):_) = node'
  where
    psSort = L.sortBy (comparing (flip pointD d . fst)) ps
    medInd = L.length psSort `div` 2
    (kMed,aMed) = psSort !! medInd
    kdLeft  = fromListWithDepth (dSucc k d) (take medInd psSort)
    kdRight = fromListWithDepth (dSucc k d) (drop (medInd + 1) psSort)
    node' = KDBranch kMed aMed d kdLeft kdRight

keys :: KDMap k a -> [k]
keys  KDEmpty                        = []
keys (KDLeaf k _ _ )                 = [k]
keys (KDBranch k _ _ kdLeft kdRight) = k : (keys kdLeft ++ keys kdRight)

insert :: (Eq k, KDKey k) => Depth -> k -> a -> KDMap k a -> KDMap k a
insert d k a KDEmpty = KDLeaf k a d
insert _ k a (KDLeaf k' a' d')
  | k == k' = KDLeaf k a d'
  | otherwise = case dimOrder k k' d' of
    LT -> KDBranch k' a' d' (KDLeaf k a (dSucc k d')) KDEmpty
    _  -> KDBranch k' a' d' KDEmpty (KDLeaf k a (dSucc k d'))
insert _ k a (KDBranch k' a' d' kdLeft kdRight)
  | k == k' = KDBranch k a d' kdLeft kdRight
  | otherwise = case dimOrder k k' d' of
    LT -> KDBranch k' a' d' (insert (dSucc k d') k a kdLeft) kdRight
    _  -> KDBranch k' a' d' kdLeft (insert (dSucc k d') k a kdRight)
