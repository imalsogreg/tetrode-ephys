{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Map.KDMap where

import Data.Maybe (maybeToList)
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Foldable as F


data KDMap k a = KDEmpty
               | KDLeaf   k a Depth
               | KDBranch k a Depth (KDMap k a) (KDMap k a)
               deriving (Eq, Show)

newtype Weight = Weight {unWeight :: Double}
                 deriving (Show, Eq, Num, Enum)

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
  dSucc p d = succ d `mod` fromIntegral (pointSize p - 1)

closer :: (Eq k, KDKey k) => Maybe (k,a) -> Maybe (k,a) -> k -> Maybe (k,a)
closer Nothing Nothing _ = Nothing
closer a Nothing _ = a
closer Nothing b _ = b
closer (Just optA@(kA,_)) (Just optB@(kB,_)) k
  | pointDistSq kA k < pointDistSq kB k = Just optA
  | otherwise                           = Just optB

--closest :: (Eq k, KDKey k) => KDMap k a -> k -> Maybe (k,a)
--closest m k = closest' 0 m k Nothing

closest :: (Eq a, Eq k, KDKey k) => k -> KDMap k a -> Maybe (k,a)
closest _ KDEmpty = Nothing
closest _ (KDLeaf k' a' _) = Just (k',a')
closest k m@(KDBranch k' a' d' kdLeft kdRight) = case dimOrder k k' d' of
  LT -> findNearest kdLeft  kdRight
  _  -> findNearest kdRight kdLeft
  where
    findNearest treeA treeB =
      let mainCandidates = case closest k treeA of
            Nothing  -> [(k',a')]
            Just (k'',a'') -> [(k',a'),(k'',a'')]
          otherCandidates
            | (pointDistSq k k') >= (pointD k d' - pointD k' d')^2 =
              maybeToList (closest k treeB)
            | otherwise = []
      in
       Just $
         L.minimumBy (comparing (pointDistSq k . fst))
         (mainCandidates ++ otherCandidates)

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

closest' :: (Eq a, Eq k, KDKey k) => k -> [KDMap k a] -> Maybe (k,a) -> Maybe (k,a)
closest' k [] closestSoFar = closestSoFar
closest' k [KDEmpty] closestSoFar = closestSoFar
closest' k ((KDLeaf k' a' d') : ms) cl@(Just(closestK, closestA)) =
  closest' k ms (closer (Just (k',a')) cl k )
closest' k ((KDLeaf k' a' d') :ms) Nothing = closest' k ms (Just (k', a'))
closest' k ((KDBranch k' a' d kdLeft kdRight) : ms) Nothing =
  closest' k ms (Just (k',a'))
closest' k ((b@(KDBranch k' a' d' kdLeft kdRight)) : ms) cl@(Just (closestSoFar,a))
  | (pointD k d' - pointD k' d')^(2::Int) > pointDistSq k closestSoFar =
    closest' k ms cl  -- TODO: Is this right? Or do I have to check that closestSoFar is closer than k'?
  | otherwise = let otherTree = if b == kdLeft then kdRight else kdLeft
                in closest' k ms (closer cl (closest k otherTree) k)


descent' :: (Eq k, KDKey k) => k -> KDMap k a -> [KDMap k a] -> [KDMap k a]
descent' _    KDEmpty acc = acc
descent' _ l@(KDLeaf _ _ _) acc = l : acc
descent' k b@(KDBranch k' _ d' kdLeft kdRight) acc = case dimOrder k k' d' of
  LT -> descent' k kdLeft  (b:acc)
  _  -> descent' k kdRight (b:acc)

{-
descent' :: (Eq k, KDKey k) => Depth -> k -> KDMap k a -> [(Depth,KDMap k a)] -> [(Depth, KDMap k a)]
descent' _ k KDEmpty acc = acc
descent' d k l@(KDLeaf _ _) acc = (d,l) : acc
descent' d k b@(KDBranch k' v' kdLeft kdRight) acc = case dimOrder k k' d of
  LT -> descent' (dSucc k d) k kdLeft  ((d,b):acc)
  _  -> descent' (dSucc k d) k kdRight ((d,b):acc)
-}

keys :: KDMap k a -> [k]
keys  KDEmpty                        = []
keys (KDLeaf k _ _ )                 = [k]
keys (KDBranch k _ _ kdLeft kdRight) = k : (keys kdLeft ++ keys kdRight)

insert :: (Eq k, KDKey k) => k -> a -> KDMap k a -> KDMap k a
insert = insert'

insert' :: (Eq k, KDKey k) => k -> a -> KDMap k a -> KDMap k a
insert' k a KDEmpty = KDLeaf k a 0
insert' k a (KDLeaf k' a' d') = case dimOrder k k' d' of
  EQ -> KDLeaf k  a  d'
  LT -> KDBranch k' a' d' (KDLeaf k a (dSucc k d')) KDEmpty
  GT -> KDBranch k' a' d' KDEmpty (KDLeaf k a (dSucc k d'))
insert' k a (KDBranch k' a' d' kdLeft kdRight) = case dimOrder k k' d' of
  LT -> KDBranch k' a' d' (insert' k a kdLeft) kdRight
  _  -> KDBranch k' a' d' kdLeft (insert' k a kdRight)
  
{-
insert' :: (Eq k, KDKey k) => Depth -> k -> a -> KDMap k a -> KDMap k a
insert' d k v  KDEmpty = KDLeaf d k v
insert' d k v (KDLeaf k' v')
  | k == k' = KDLeaf k v'
  | pointD k d < pointD k' d =
    KDBranch dim k' v' (KDLeaf  (dSucc k d) k v) KDEmpty
  | otherwise =
    KDBranch dim k' v' KDEmpty (KDLeaf (dSucc k v) k v)
insert' _ k w v (KDBranch dim w' k' v' kdLeft kdRight)
  | k == k' = KDBranch dim w k' v kdLeft kdRight
  | pointD k dim < pointD k' dim =
    KDBranch dim w' k' v' (insert' (dim + 1 `mod` pointSize k - 1) k w v kdLeft) kdRight
  | otherwise =
    KDBranch dim w' k' v' kdLeft (insert' (dim+1 `mod` pointSize k - 1) k w v kdRight)
-}




{-
instance Fractional Weight where
  Weight a / Weight b = Weight $ a / b
  recip (Weight a) = Weight $ recip a
  fromRational a = Weight $ fromRational a

instance Num Weight where
  Weight a + Weight b = Weight $ a + b
  Weight a - Weight b = Weight $ a - b
  Weight a * Weight b = Weight $ a * b
  negate (Weight a) = Weight $ negate a
  abs (Weight a) = Weight $ abs a
  signum (Weight a) = Weight $ signum a
  fromInteger a = Weight (fromIntegral a)

instance Floating Weight where
  Weight a ** Weight b = Weight $ a ** b
  sqrt (Weight a) = Weight $ sqrt a
  exp  (Weight a) = Weight $ exp a
  log  (Weight a) = Weight $ log a
  Weight a `logBase` Weight b = Weight $ logBase a b
-}
