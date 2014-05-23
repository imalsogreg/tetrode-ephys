{-# LANGUAGE BangPatterns #-}

module Data.Map.KDMap where

import Data.Ord (comparing)
import qualified Data.Foldable as F


data KDMap k a = KDEmpty
               | KDLeaf   !Depth !Weight k a
               | KDBranch !Depth !Weight k a (KDMap k a) (KDMap k a)


type Weight = Double
type Depth  = Int


class KDKey k where
  pointD       :: k -> Int -> Double
  pointSize    :: k -> Int

  pointDDistSq :: k -> k -> Int -> Double
  pointDDistSq a b i = (pointD b i - pointD a i)^(2::Int)

  pointDistSq  :: k -> k -> Double
  pointDistSq a b = sum $ map (pointDDistSq a b)
                    [0..pointSize a - 1]

  dimOrder :: k -> k -> Int -> Ordering
  dimOrder a b n = compare (pointD a n) (pointD b n)

instance F.Foldable (KDMap k) where
  foldr _ z KDEmpty = z
  foldr f z (KDLeaf _ _ _ a) = f a z
  foldr f z (KDBranch _ _ _ v kdLeft kdRight) = F.foldr f (f v (F.foldr f z kdLeft)) kdRight


newtype Point2 = Point2 {unPoint2 :: (Double,Double)}
                 deriving (Show,Eq)


instance KDKey Point2 where
  pointD (Point2 (x,_)) 0 = x
  pointD (Point2 (_,y)) 1 = y
  pointD _              n = error $ "Point2 out of bounds index: " ++ show n
  pointSize _    = 2

closer :: (Eq k, KDKey k) => Maybe (k,Weight,a) -> Maybe (k,Weight,a) -> k -> Maybe (k,Weight,a)
closer a Nothing _ = a
closer Nothing b _ = b
closer (Just optA@(kA,wA,aA)) (Just optB@(kB,wB,aB)) k
  | pointDistSq kA k < pointDistSq kB k = Just optA
  | otherwise                           = Just optB

closest :: (Eq k, KDKey k) => KDMap k a -> k -> Maybe (k,Weight,a)
closest m k = closest' 0 m k Nothing

closest' :: (Eq k, KDKey k) => Int -> KDMap k a -> k -> Maybe (k,Weight,a) -> Maybe (k,Weight,a)
closest' d KDEmpty _ closestSoFar = closestSoFar
closest' d (KDLeaf _ w' k' a') k closestSoFar = closer (Just (k',w',a')) closestSoFar k
closest' d (KDBranch _ w' k' a' kdLeft kdRight) k closestSoFar
  | k == k' = Just (k,w',a')
  | dimOrder k k' d == LT =
    closest' (d+1 `mod` pointSize k - 1) kdLeft k (closer (Just (k',w',a')) closestSoFar k)
  | otherwise =
      closest' (d+1 `mod` pointSize k - 1) kdRight k (closer (Just (k',w',a')) closestSoFar k)
    

keys :: KDMap k a -> [(k,Double)]
keys  KDEmpty                          = []
keys (KDLeaf _ w k _)                  = [(k,w)]
keys (KDBranch _ w k _ kdLeft kdRight) = (k,w) : (keys kdLeft ++ keys kdRight)


insert :: (Eq k, KDKey k) => k -> Weight -> a -> KDMap k a -> KDMap k a
insert = insert' 0 


insert' :: (Eq k, KDKey k) => Int -> k -> Weight -> a -> KDMap k a -> KDMap k a
insert' d k w v  KDEmpty = KDLeaf d w k v
insert' _ k w v (KDLeaf dim w' k' v')
  | k == k' = KDLeaf dim w k  v'
  | pointD k dim < pointD k' dim =
    KDBranch dim w' k' v' (KDLeaf  (dim + 1 `mod` pointSize k - 1) w k v) KDEmpty
  | otherwise =
    KDBranch dim w' k' v' KDEmpty (KDLeaf (dim + 1 `mod` pointSize k - 1) w k v)
insert' _ k w v (KDBranch dim w' k' v' kdLeft kdRight)
  | k == k' = KDBranch dim w k' v kdLeft kdRight
  | pointD k dim < pointD k' dim =
    KDBranch dim w' k' v' (insert' (dim + 1 `mod` pointSize k - 1) k w v kdLeft) kdRight
  | otherwise =
    KDBranch dim w' k' v' kdLeft (insert' (dim+1 `mod` pointSize k - 1) k w v kdRight)


