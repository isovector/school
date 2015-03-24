-- Decided this would be a good time to learn haskell. Spent a lot of time cursing.

import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)

import Control.Monad (forM_)

discount = 0.9

data Action = ALeft | ARight | AUp | ADown deriving (Show, Eq)
type Pos = (Int, Int)
type Utility = (Pos, Float)

-- gets a new position given a pos and an action
go :: Pos -> Action -> Pos
go (x, y) ALeft  = (x - 1, y)
go (x, y) ARight = (x + 1, y)
go (x, y) AUp    = (x, y - 1)
go (x, y) ADown  = (x, y + 1)

-- get the utility of a position
utilOf :: [Utility] -> Pos -> Float
utilOf u p = snd
           $ head
           $ filter ((== p) . fst) u

-- value iteration
viterate :: (Pos -> [Action])               -- action generation
         -> (Pos -> Pos -> Action -> Float) -- transition model
         -> (Pos -> Float)                  -- reward function
         -> [Utility]                       -- current state
         -> [Utility]                       -- output: updated state
viterate as tm r u = map compute u
  where compute :: Utility -> Utility       -- get the new utility for a given position
        compute (p, _) = (p, r p + discount * max)
                                            -- max over actions of sum of neighbor state's expected utility
          where max = maximum . map eUtil . as $ p
                eUtil :: Action -> Float    -- expected utility of taking an action from here
                eUtil a = let p' = go p a
                           in tm p p' a * utilOf u p'

-- initial state
world = [ ((1, 1), 0), ((1, 2), 0), ((1, 3), 0),
          ((2, 1), 0), ((2, 2), 0), ((2, 3), 0),
          ((3, 1), 0), ((3, 2), 0), ((3, 3), 0) ]

-- given a position, see which actions we can take
actionGen :: Pos -> [Action]
actionGen (x, y) = catMaybes [
    action (x > 1) ALeft,
    action (x < 3) ARight,
    action (y > 1) AUp,
    action (y < 3) ADown     ]
            -- helper to return the action iff the predicate is true
      where action :: Bool -> Action -> Maybe Action
            action True a  = Just a
            action False _ = Nothing

-- transition model
transModel :: Pos -> Pos -> Action -> Float
transModel p p' a | go p a == p'     = 0.8  -- going in the right direction has p = 0.8
                  | go p a == neg p' = 0    -- going opposite is p = 0
                  | otherwise        = 0.1  -- since we only check neighbors, it is otherwise p = 0.1
                    where neg (x, y) = (-x, -y)

-- determine best policy
policy :: (Pos -> [Action]) -- action generator
       -> [Utility]         -- state
       -> String            -- graphical representation of the policy
policy as u = map snd
            . sortBy (comparing posToInt)
            $ map forward u
        -- ensure our positions are in the right order
  where posToInt ((x, y), _) = x + y * 10

        -- helper function to make the types line up
        forward :: Utility -> (Pos, Char)
        forward (p, _) = (p, asArrow p)

        -- return an arrow pointing to the highest utility neighbor
        asArrow :: Pos -> Char
                    -- if going in a direction results in the best node,
                    -- that is the arrow we return
        asArrow p | go p ALeft  == best = '←'
                  | go p ARight == best = '→'
                  | go p ADown  == best = '↓'
                  | go p AUp    == best = '↑'
                    -- go in the direction of the maximum utility attained by
                    -- going in each action generated for this position
                    where best = go p
                               . maximumBy (comparing $ utilOf u . go p )
                               $ as p

-- reward function
reward :: Float -> Pos -> Float
reward r (1, 1) = r     -- top left has reward r
reward _ (3, 1) = 10    -- top right has reward 10
reward _ _      = -1    -- otherwise -1

-- put a newline into a string every 3 characters
split3 :: String -> String
split3 (x:y:z:xs) = [x,y,z,'\n'] ++ split3 xs
split3 _          = []

-- print a message without invoking the IO monad
showTrace :: (Show a) => a -> a
showTrace a = trace (show a) a

-- compute and print out the optimal policy for a given r
doIt r = do putStrLn $ show r
            putStrLn . split3
                     . policy actionGen     -- compute the policy
                     . showTrace            -- print out the utilities
                     . last                 -- get the result
                     . take 25              -- iterate 25 times
                     $ iterate              -- (lazily) while true...
                        (viterate actionGen transModel $ reward r)
                        world               -- starting state

-- solve the assignment
main = forM_ [100, -3, 0, 3] $ \r -> do doIt r
                                        putStrLn ""
