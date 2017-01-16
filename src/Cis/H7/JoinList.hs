module JoinList
    (
    ) where

import Sized

data JoinList m a = Empty
                 | Single m a
                 | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (mappend (tag a) (tag b)) a b

getSized :: Sized s => s -> Int
getSized = getSize . size

sizeJ :: (Sized s, Monoid s) => JoinList s a -> Int
sizeJ = getSized . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single m a) | i == 0 = Just a
                      | otherwise = Nothing
indexJ i (Append m l r) | i >= getSized m = Nothing
                        | i >= sizeJ l = indexJ (i - sizeJ l) r
                        | otherwise = indexJ i l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 l = l
dropJ n l | n >= sizeJ l = Empty
dropJ n (Append _ l r) | n >= sizeJ l = dropJ (n - sizeJ l) r
                       | otherwise = dropJ n l +++ r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 l = Empty
takeJ n l | n >= sizeJ l = l
takeJ n (Append _ l r) | n <= sizeJ l = takeJ n l
                       | otherwise = l +++ takeJ (sizeJ r - n) r
