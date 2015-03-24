import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)

import Control.Monad (forM_)

discount = 0.9

data Action = ALeft | ARight | AUp | ADown deriving (Show, Eq)
type Pos = (Int, Int)
type Utility = (Pos, Float)

go :: Pos -> Action -> Pos
go (x, y) ALeft  = (x - 1, y)
go (x, y) ARight = (x + 1, y)
go (x, y) AUp    = (x, y - 1)
go (x, y) ADown  = (x, y + 1)

utilOf :: [Utility] -> Pos -> Float
utilOf u p = snd $ head $ filter ((== p) . fst) u

viterate :: (Pos -> [Action])
         -> (Pos -> Pos -> Action -> Float)
         -> (Pos -> Float)
         -> [Utility]
         -> [Utility]
viterate as tm r u = u'
  where u' = map compute u
        compute :: Utility -> Utility
        -- should i do something with V?
        compute (p, v) = (p, r p + discount * max)
          where max = maximum $ map eUtil actions
                eUtil :: Action -> Float
                eUtil a = let p' = go p a
                           in tm p p' a * utilOf u p'
                actions = as p

world = [ ((1, 1), 0),
          ((1, 2), 0),
          ((1, 3), 0),
          ((2, 1), 0),
          ((2, 2), 0),
          ((2, 3), 0),
          ((3, 1), 0),
          ((3, 2), 0),
          ((3, 3), 0) ]

actionGen :: Pos -> [Action]
actionGen (x, y) = catMaybes $ [
    action (x > 1) ALeft,
    action (x < 3) ARight,
    action (y > 1) AUp,
    action (y < 3) ADown ]
      where action :: Bool -> Action -> Maybe Action
            action True a  = Just a
            action False _ = Nothing

transModel :: Pos -> Pos -> Action -> Float
transModel p p' a | go p a == p'     = 0.8
                  | go p a == neg p' = 0
                  | otherwise        = 0.1
                    where neg (x, y) = (-x, -y)

showTrace :: (Show a) => a -> a
showTrace a = trace (show a) a


policy :: (Pos -> [Action]) -> [Utility] -> [Char]
policy as u = map snd . sortBy (comparing posToInt) $ map getArrow u
  where posToInt ((x, y), _) = x + y * 10
        getArrow :: Utility -> (Pos, Char)
        getArrow (p, _) = (p, asArrow p)
        asArrow :: Pos -> Char
        asArrow p | go p ALeft  == best = '←'
                  | go p ARight == best = '→'
                  | go p ADown  == best = '↓'
                  | go p AUp    == best = '↑'
                    where best = go p . maximumBy (comparing cmp) $ as p
                          cmp = utilOf u . go p

reward :: Float -> Pos -> Float
reward r (1, 1) = r
reward _ (3, 1) = 10
reward _ _ = -1

split3 :: String -> String
split3 (x:y:z:xs) = [x,y,z,'\n'] ++ split3 xs
split3 _ = []

doIt r = do putStrLn $ show r
            putStrLn . split3 . policy actionGen . showTrace . last . take 25 $
              iterate (viterate actionGen transModel $ reward r) world
            return ()

main = forM_ [100, -3, 0, 3] $ \r -> do doIt r
                                        putStrLn ""
