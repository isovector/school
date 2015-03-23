{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (nub, group, sort, minimumBy, maximumBy, partition)
import Debug.Trace (trace)
import Data.Ord (comparing)
import Control.Monad (join)
import Control.Arrow ((***))


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
percentageOf f xs = intDiv (length satisfying) $ length xs
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

between :: [Float] -> [Float]
between []           = []
between (x:[])       = []
between (x:xs@(y:_)) = (x + y) / 2 : (between xs)



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
attrCount :: [Example] -> Int
attrCount ((Ex as _):_) = length as

attrsOf :: [Example] -> [Int]
attrsOf ed = map fst $ filter ((>1) . snd) counts
  where possible = [0 .. attrCount ed - 1]
        counts = map (\x -> (x, length $ attrValues ed x)) possible



--------------- math
attrPartition :: [Example] -> Attr -> Float -> ([Example],[Example])
attrPartition ed a thresh = partition (attrCmp a (< thresh)) ed

-- compute the information gain for a
gain :: [Example] -> Attr -> Float -> Float
gain ed a thresh = entropy p total - remainder
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

        remi :: (Float -> Float -> Bool) -> Float
        remi f = intDiv totali total * entropy pi' totali
          where samples = filter (attrCmp a (f thresh)) ed
                totali = length samples
                pi' = length $ filter c12n samples

        remainder = remi (<) + remi (>)


ttrace :: Show a => a -> a
ttrace a = trace (show a) a


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
    where subtree :: [Example] -> DecisionTree
          subtree ed' = dtl ed' $ attrsOf ed'
          (less, more) = join (***) subtree $ attrPartition ed a thresh

          bestAttrGain :: Attr -> Float
          bestAttrGain a = maximumBy (comparing $ gain ed a) . between . attrValues ed $ a

          a = maximumBy (comparing bestAttrGain) as
          thresh = ttrace $ bestAttrGain a

horseAttrs = [
    "K",
    "Na",
    "CL",
    "HCO3",
    "Endotoxin",
    "Aniongap",
    "PLA2",
    "SDH",
    "GLDH",
    "TPP",
    "Breath rate",
    "PCV",
    "Pulse rate",
    "Fibrinogen",
    "Dimer",
    "FibPerDim"
    ]


runTest :: String -> String -> IO Float
runTest f t = do putStrLn $ f ++ ":"
                 trainFile <- readFile (f ++ "-train.txt")
                 testFile <- readFile (f ++ "-test.txt")

                 let c6yp = (== t) -- which samples are classified True
                     trainData = strToExamples trainFile c6yp
                     testData = strToExamples testFile c6yp
                     dt = dtl trainData $ attrsOf trainData

                 {-return 0.0-}

                 putStrLn . unlines $ map (show . flip (gain trainData) 100) $ attrsOf trainData
                 return $ percentageOf (goodC6y dt) testData

nain = do test <- readFile "test-train.txt"
          let trainData = strToExamples test (== "good")
          print $ dtl trainData $ attrsOf trainData

main = do horse <- runTest "horse" "colic."
          print horse
