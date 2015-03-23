{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (nub, group, sort, minimumBy, maximumBy)
import Debug.Trace (trace)
import Data.Ord (comparing)


--------------- datatypes
data Example = Ex [Float] Bool deriving (Show, Eq, Read)
type Attr = Int

data DecisionTree = Tree Attr Float DecisionTree DecisionTree
                  | Classification Bool
                  deriving (Read, Show)



--------------- helper functions
-- split a string by a predicate
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                   "" -> []
                   s' -> w : wordsWhen p s''
                     where (w, s'') = break p s'

-- what percentage of a list satisfy a predicate?
percentageOf :: (a -> Bool) -> [a] -> Float
percentageOf f xs = intDiv (length satisfying) (length xs)
  where satisfying = filter f xs

-- helper to do floating division on ints
intDiv :: Int -> Int -> Float
intDiv a b = fromIntegral a / fromIntegral b

-- compute the mode of a list
mode :: Ord a => [a] -> a
mode xs = head . maximumBy (comparing length) . group $ sort xs

-- compute the median of a list
median :: Ord a => [a] -> a
median xs = (sort xs) !! (div (length xs) 2)



--------------- classifications
-- classify an example
c6y :: DecisionTree -> Example -> Bool
c6y (Tree a thresh less more) e = if getAttr a e <= thresh
                                     then c6y less e
                                     else c6y more e
c6y (Classification r) _ = r

-- is it a good classification?
goodC6y :: DecisionTree -> Example -> Bool
goodC6y dt e = c6y dt e == c12n e

-- get the classification of an example
c12n :: Example -> Bool
c12n (Ex _ c) = c



--------------- parsing
parseExample :: String -> (String -> Bool) -> Example
parseExample s p = Ex (map read $ init splits) (p $ last splits)
  where splits = wordsWhen (== ',') s

strToExamples :: String -> (String -> Bool) -> [Example]
strToExamples s f = map (flip parseExample $ f) $ lines s



--------------- attribute helpers
getAttr :: Attr -> Example -> Float
getAttr a (Ex as _) = as !! a

-- predicate builder to see if an attribute should be filtered
attrCmp :: Attr -> (Float -> Bool) -> Example -> Bool
attrCmp a f e = f $ getAttr a e

-- get distinct values for an attribute
attrValues :: [Example] -> Int -> [Float]
attrValues ed a = sort . nub $ map (getAttr a) ed

-- get number of attributes for an example
numAttrs :: Example -> Int
numAttrs (Ex as _) = length as



--------------- math
-- compute the information gain for a
gain :: [Example] -> Attr -> Float
gain ed a = entropy p total - remainder
  where p = length $ filter c12n ed
        total = length $ ed

        entropy :: Int -> Int -> Float
        entropy p' total' = (log2 p' total') + log2 n total'
          where n = total' - p'
                -- compute -x*log2(x)
                log2 :: Int -> Int -> Float
                log2 0 _ = 0
                log2 x t = -prec * logBase 2 prec
                  where prec = intDiv x t

        remainder = sum $ map remi [0 .. length values - 1]
        values = attrValues ed a

        remi :: Int -> Float -- remainder for an attribute index
        remi i = (intDiv totali total) - entropy pi' totali
          where v = values !! i
                -- examples with this attribute value
                samples = filter (attrCmp a (== v)) ed
                totali = length samples
                pi' = length $ filter c12n samples

epsilon = 0.0001



--------------- brass tacks
-- build a decision tree
dtl :: [Example] -> [Attr] -> DecisionTree
dtl [] _                           = Classification False
dtl ed@(ex:_) as
  -- all the classifications are the same
  | all (== c12n ex) $ map c12n ed = Classification $ c12n ex
  -- the attributes are empty
  | as == []                       = Classification . mode $ map c12n ed
  | otherwise                      = Tree a thresh less more
    where subtree :: (Float -> Float -> Bool) -> DecisionTree
          -- compute a subtree given a function for comparing thresholds
          subtree f = dtl (filter (attrCmp a $ flip f $ thresh) ed) as
          less = subtree (<=)
          more = subtree (>)
          -- get the best attribute in terms of information gain
          a = minimumBy (comparing $ gain ed) [0 .. length as - 1]
          values = attrValues ed a
          -- take the median - epsilon for the threshold
          thresh = (median $ map (getAttr a) ed) - epsilon


main = do trainFile <- readFile "horse-train.txt"
          testFile <- readFile "horse-test.txt"

          let c6yp = (== "colic.") -- which samples are classified True
              trainData = strToExamples trainFile c6yp
              testData = strToExamples testFile c6yp
              -- attributes existing in the examples
              as = [0 .. (numAttrs $ trainData !! 0) - 1]
              dt = dtl trainData as
          print $ percentageOf (goodC6y dt) testData
