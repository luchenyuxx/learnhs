module HeapSort where

import           Control.Monad (liftM2)
import           Data.Maybe    (fromJust, isNothing)

heapSort :: Ord a => [a] -> [a]
heapSort l = sortHeap $ makeHeap l

sortHeap :: Ord a => [a] -> [a]
sortHeap [] = []
sortHeap [a] = [a]
sortHeap (a:as) = sortHeap (checkRoot $ promoteLast as) ++ [a]

promoteLast :: [a] -> [a]
promoteLast [] = []
promoteLast l = last l : withoutLast l

withoutLast :: [a] -> [a]
withoutLast [] = []
withoutLast [a] = []
withoutLast (a:as) = a:withoutLast as

makeHeap :: Ord a => [a] -> [a]
makeHeap = foldl (\l a -> checkLast $ putToLast a l) []

checkRoot :: Ord a => [a] -> [a]
checkRoot [] = []
checkRoot l = fromJust $ checkChild l 0

checkChild :: Ord a => [a] -> Integer -> Maybe [a]
checkChild l rootIndex | isNothing $ greaterHeapChildIndex l rootIndex = Just l
                       | otherwise = do
                           stgc <- isSmallerThanGreaterChild l rootIndex
                           if stgc then do ci <- greaterHeapChildIndex l rootIndex
                                           nl <- swapWithHeapParent l ci
                                           checkChild nl ci
                           else Just l

isSmallerThanGreaterChild :: Ord a => [a] -> Integer -> Maybe Bool
isSmallerThanGreaterChild l rootIndex = do
  cv <- greaterChild l rootIndex
  rv <- get l rootIndex
  return (rv < cv)

greaterChild :: Ord a => [a] -> Integer -> Maybe a
greaterChild l rootIndex = greaterHeapChildIndex l rootIndex >>= get l

greaterHeapChildIndex :: Ord a => [a] -> Integer -> Maybe Integer
greaterHeapChildIndex l i | i<0 || i> lastIndex l || (2*i+1) >lastIndex l = Nothing
greaterHeapChildIndex l i | (2*i+2)>lastIndex l = Just (2*i+1)
greaterHeapChildIndex l i = do
  cl <- get l (2*i+1)
  cr <- get l (2*i+2)
  if cl>cr then Just (2*i+1) else Just (2*i+2)

lastIndex :: [a] -> Integer
lastIndex l = length' l -1

checkLast :: Ord a => [a] -> [a]
checkLast [] = []
checkLast l = fromJust $ checkParent l $ lastIndex l

checkParent :: Ord a => [a] -> Integer -> Maybe [a]
checkParent l childIndex | childIndex == 0 = Just l
                         | childIndex < 0 = Nothing
                         | otherwise = do
                             ltp <- isLargerThanHeapParent l childIndex
                             if ltp then do pi <- parentIndex childIndex
                                            nl <- swapWithHeapParent l childIndex
                                            checkParent nl pi
                             else Just l

putToLast :: Ord a => a -> [a] -> [a]
putToLast a = foldr (:) [a]

isLargerThanHeapParent :: Ord a => [a] -> Integer -> Maybe Bool
isLargerThanHeapParent l childIndex = do
  p <- parent l childIndex
  c <- get l childIndex
  return (c > p)

swapWithHeapParent :: Ord a => [a] -> Integer -> Maybe [a]
swapWithHeapParent l childIndex = do
  pi <- parentIndex childIndex
  swap l pi childIndex

swap :: [a] -> Integer -> Integer -> Maybe [a]
swap l i1 i2 | i1==i2 = Just l
             | otherwise = do
                 v1 <- get l i1
                 v2 <- get l i2
                 l2 <- set l i1 v2
                 set l2 i2 v1

parentIndex :: Integer -> Maybe Integer
parentIndex i | i < 0 = Nothing
                 | otherwise = Just $ floor $ fromInteger (i-1) / 2

parent :: Ord a => [a] -> Integer -> Maybe a
parent l i = parentIndex i >>= get l

length' :: [a] -> Integer
length' [] = 0
length' (a:as) = length' as + 1

get :: [a] -> Integer -> Maybe a
get [] _ = Nothing
get (a:as) i | i < 0 = Nothing
           | i == 0 = Just a
           | otherwise = get as (i-1)

set :: [a] -> Integer -> a -> Maybe [a]
set [] _ _ = Nothing
set (a:as) i v | i==0 = Just (v:as)
               | i<0 = Nothing
               | otherwise = do
                   rest <- set as (i-1) v
                   return (a:rest)
