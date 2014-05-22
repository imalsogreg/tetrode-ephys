module Data.Map.KDMap where

data KDMap a = KDEmpty
             | KDBranch !Int (KDMap a) (KDMap a) a

class KDKey k where
  pointD       :: k -> Int -> Double
  pointSize    :: k -> Int

  pointDDistSq :: k -> k -> Int -> Double
  pointDDistSq a b i = (pointD b i - pointD a i)^(2::Int)

  pointDistSq  :: k -> k -> Double
  pointDistSq a b = sum $ map (pointDDistSq a b)
                    [0..pointSize a - 1]

insert :: (KDKey k) => Int -> k -> a -> KDMap k a -> KDMap k a
insert d k v KDEmpty = KDBranch d KDEmpty KDEmpty
insert k v m = go 0 m
  where go n = let s = 
          | pointD 
