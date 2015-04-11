{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (nub, group, sort, maximumBy, partition)
import Debug.Trace (trace)
import Data.Ord (comparing)
import Control.Monad (join, ap)
import Control.Arrow ((***))

import Control.Applicative ((<$>), (<*>))
import Control.Monad.State
import Control.Monad.Writer



--------------- datatypes
data Example = Ex [Float] Bool deriving (Show, Eq, Read)
type Attr = Int
type StateWriter = StateT Int (Writer [String])

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
percentageOf f xs = intDiv (length $ filter f xs) $ length xs

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
c6y (Classification r) _        = r
c6y (Tree a thresh less more) e = if getAttr a e <= thresh
                                     then c6y less e
                                     else c6y more e

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

        -- compute remainder for each half of the dataset
        remainder = remi (<) + remi (>)

        remi :: (Float -> Float -> Bool) -> Float
        remi f = intDiv total' total * entropy pi' total'
          where samples = filter (attrCmp a (f thresh)) ed
                total' = length samples
                pi' = length $ filter c12n samples


-- given [x], get [(x, f x)]
zipap :: (a -> b) -> [a] -> [(a, b)]
zipap _ []     = []
zipap f (x:xs) = (x, f x) : zipap f xs

--------------- brass tacks
-- build a decision tree
dtl :: [Example] -> [Attr] -> DecisionTree
dtl [] _                           = Classification False
dtl ed@(ex:_) as
    -- all the classifications are the same
  | all (== c12n ex) $ map c12n ed = Classification $ c12n ex
    -- the attributes are empty
  | as == []                       = Classification . mode $ map c12n ed
    -- recursive step
  | otherwise                      = Tree a thresh less more
    where subtree :: [Example] -> DecisionTree
          subtree = ap dtl attrsOf

          -- partition the remaining datapoints
          -- into left and right trees
          (less, more) = join (***) subtree
                       $ attrPartition ed a thresh

          -- get the best threshod for an attribute
          bestThresh :: Attr -> Float
          bestThresh a = maximumBy
                            (comparing $ gain ed a) -- best gain
                       . between                    -- get midpoints
                       . attrValues ed              -- get unique values
                       $ a

          (a, thresh) = maximumBy (comparing (\(a', t) -> gain ed a' t))
                        -- zip each attribute with its best threshold
                      $ zipap bestThresh as

-- pretty print a decision tree
pretty :: [String] -> DecisionTree -> IO ()
pretty names = pretty' 0
  where pretty' :: Int -> DecisionTree -> IO ()
        pretty' i (Classification r) = do putStrLn $ align i ++ "→ " ++ show r
        pretty' i (Tree a t l m)     = do putStrLn $ align i ++ label names a t
                                          pretty' (i + 1) l
                                          pretty' (i + 1) m
        align i = concat $ replicate i "|  "

-- the label for a decision node
label :: [String] -> Attr -> Threshold -> String
label n a t = (n!! a) ++ " < " ++ show t

-- user function for getting a dot file for the decision tree
genDotRealsies :: [String]      -- names of attrs
               -> DecisionTree
               -> [String]
genDotRealsies n dt = let w = runStateT (genDot n dt) 0         -- rip the writer out of the state
                          p = runWriter w                       -- rip a pair out of the writer
                          in ["digraph g {"] ++ snd p ++ ["}"]  -- put the necessary header and footer on

genDot :: [String]                          -- attr names
       -> DecisionTree                      -- dt to print
       -> StateT Int (Writer [String]) ()   -- monad which keeps track of ids, and can output lines
genDot n (Tree a t l r) = do genDot n l     -- generate left side
                             lid <- get
                             put $ lid + 1
                             genDot n r     -- generate right side
                             rid <- get
                             let id' = rid + 1
                             put id'
                             let is = "A" ++ show id'
                                 ls = "A" ++ show lid
                                 rs = "A" ++ show rid
                                 ts = show t
                             tell $ [
                                    is ++ " [label=\"" ++ (n !! a) ++ "\"];",
                                    is ++ " -> " ++ ls ++ " [label=\"< " ++ ts ++ "\"]",
                                    is ++ " -> " ++ rs ++ " [label=\"> " ++ ts ++ "\"]"
                                    ]
genDot n (Classification r) = do id' <- ("A" ++) . show <$> get
                                 tell . return $ id' ++ " [label=\"" ++ show r ++ "\"];"


-- names for attributes for horse data set
horseAttrs = [ "K", "Na", "CL", "HCO3", "Endotoxin", "Aniongap", "PLA2",
             "SDH", "GLDH", "TPP", "Breath rate", "PCV", "Pulse rate",
             "Fibrinogen", "Dimer", "FibPerDim" ]

-- names for attributes for math data set
mathAttrs = [ "School", "Sex", "Age", "Home", "FamSize", "Cohab", "MomEdu",
            "DadEdu", "Primary", "ParentSex", "Travel", "Study", "Fails",
            "ExtraEdu", "FamEdu", "EdtraPaid", "ExtraCuric", "Nursery",
            "WantsEdu", "Internet", "Relationship", "FamQuality", "FreeTime",
            "GoingOut", "AlcoSchool", "AlcoWeekend", "Health", "DaysMissed" ]

runTest :: Bool         -- want graphviz output?
        -> String       -- dataset
        -> String       -- value for success classifications
        -> [String]     -- attr names
        -> IO ()
runTest g f t n = do trainFile <- readFile (f ++ "-train.txt")
                     testFile <- readFile (f ++ "-test.txt")
                     let c6yp = (== t) -- which samples are classified True
                         trainData = strToExamples trainFile c6yp
                         testData = strToExamples testFile c6yp
                         dt = dtl trainData $ attrsOf trainData
                     if g
                        -- show graphviz output
                        then putStrLn . unlines $ genDotRealsies n dt
                        -- otherwise pretty print it and give the % classified
                        else do pretty n dt
                                print . length . filter (goodC6y dt) $ trainData
                                print . length . filter (goodC6y dt) $ testData

main = runTest False "horse" "colic." horseAttrs

