module Party where

import Employee
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (empFun e + f)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1++es2) (f1+f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a ls) = f a $ map (treeFold f) ls

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e ls = glCons e $ mconcat ls

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e ls = (combineGLs e $ map snd ls, mconcat $ map (uncurry moreFun) ls)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

format :: GuestList -> String
format (GL es f) = "Fun score : "
                ++ show f ++ "\n"
                ++ unlines (sort $ map empName es)

main :: IO ()
main = readFile "./src/company.txt" >>=
  putStrLn . format . maxFun . read
