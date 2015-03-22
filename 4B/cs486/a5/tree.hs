{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (nub, group, sort, minimumBy, maximumBy)
import Debug.Trace (trace)
import Data.Ord (comparing)

data Example = Ex [Float] Bool deriving (Show, Eq)
type ExData = [Example]
type Attr = Int

data DecisionTree = Tree Attr Float DecisionTree DecisionTree
                  | Classification Bool
                  deriving Show

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                   "" -> []
                   s' -> w : wordsWhen p s''
                     where (w, s'') = break p s'

parseExample :: String -> (String -> Bool) -> Example
parseExample s p = Ex (map read $ init splits) (p $ last splits)
  where splits = wordsWhen (== ',') s

intDiv :: Int -> Int -> Float
intDiv a b = fromIntegral a / fromIntegral b

getAttr :: Attr -> Example -> Float
getAttr a (Ex as _) = as !! a

attrCmp :: Attr -> (Float -> Bool) -> Example -> Bool
attrCmp a f e = f $ getAttr a e

gain :: ExData -> Attr -> Float
gain ed a = entropy p total - remainder
  where p = length $ filter c12n ed
        total = length $ ed

        entropy :: Int -> Int -> Float
        entropy p total = (log2 p total) + log2 n total
          where n = total - p
                log2 :: Int -> Int -> Float
                log2 0 _ = 0
                log2 x total = -prec * logBase 2 prec
                  where prec = intDiv x total

        remainder = sum $ map rem [0 .. length values - 1]
        values = nub $ map (getAttr a) ed

        rem :: Int -> Float
        rem i = (intDiv totali total) - entropy pi totali
          where v = values !! i
                samples = filter (attrCmp a (== v)) ed
                totali = length samples
                pi = length $ filter c12n samples


-- classification of an example
c12n :: Example -> Bool
c12n (Ex _ c) = c

mode :: Ord a => [a] -> a
mode xs = head . maximumBy (comparing length) . group $ sort xs

median :: Ord a => [a] -> a
median xs = (sort xs) !! (div (length xs) 2)

ttrace :: Show a => a -> a
ttrace a = trace (show a) a

dtl :: ExData -> [Attr] -> DecisionTree
dtl [] _                           = Classification False
dtl ed@(ex:_) as
  | all (== c12n ex) $ map c12n ed = Classification $ c12n ex
  | as == []                       = Classification . mode $ map c12n ed
  | otherwise                      = Tree a thresh less more
    where subtree :: (Float -> Float -> Bool) -> DecisionTree
          subtree f = dtl (filter (attrCmp a $ f thresh) ed) as
          less = subtree (<=)
          more = subtree (>)
          a = minimumBy (comparing $ gain ed) [0 .. length as]
          thresh = median $ map (getAttr a) ed




main = do s <- readFile "test-train.txt"
          let ed = map (flip parseExample $ (== "colic.")) $ lines s
          let as = [0]
          print $ dtl ed as
          -- putStrLn . unlines . map show $ map (gain ed) [0..1]
          -- putStrLn . unlines $ map show ed



{-In the decision tree, use only binary tests, i.e. each node should test whether-}
{-a particular attribute has a value greater or smaller than a threshold. In-}
{-deciding which attribute to test at any point, use the information gain metric.-}
{-Set the node test threshold for each potential attribute using this same metric-}
{-i.e. at each point, see all the values that exist for a particular attribute in-}
{-the remaining instances, order those values, and try threshold values that are-}
{-(half way) between successive attribute values. Use the threshold value that-}
{-gives the highest information gain. Allow the same attribute to be tested again-}
{-later in the tree (with a different threshold). This means that along a path-}
{-from the root to a leaf, the same attribute might be tested multiple times.-}
{-After learning the decision tree, use the horseTest file to test the-}
{-generalization accuracy of the tree.-}
