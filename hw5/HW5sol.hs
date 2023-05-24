-- Zachary Chand , Dylan Mitchel Karambut 
-- CS381 
-- Tues , May 23
-- Homework 5
module HW5sol where
import HW5types
-- Function to compute the rank of a command
rankC :: Cmd -> CmdRank
rankC (LDI _) = (0, 1)        -- LDI command: consumes 0 items, produces 1 item
rankC ADD = (2, 1)            -- ADD command: consumes 2 items, produces 1 item
rankC MULT = (2, 1)           -- MULT command: consumes 2 items, produces 1 item
rankC DUP = (1, 2)            -- DUP command: consumes 1 item, produces 2 items
rankC DEC = (1, 1)            -- DEC command: consumes 1 item, produces 1 item
rankC SWAP = (2, 2)           -- SWAP command: consumes 2 items, produces 2 items
rankC (POP n) = (n, 0)        -- POP command: consumes n items, produces 0 items
rankC (IFELSE _ _) = (0, 0)   -- IFELSE command: handled separately in rankP, consumes 0 items, produces 0 items
rankC (LDB _) = (0, 1)        -- LDB command: consumes 0 items, produces 1 item
rankC LEQ = (2, 1)            -- LEQ command: consumes 2 items, produces 1 item

-- Function to compute the rank of a program
rankP :: Prog -> Rank -> Maybe Rank
rankP prog rank = rank' prog rank
  where
    rank' :: Prog -> Rank -> Maybe Rank
    rank' [] r = Just r
    rank' (cmd:cmds) r = case rankCmd cmd r of
      Just newRank -> rank' cmds newRank
      Nothing -> Nothing
    
    rankCmd :: Cmd -> Rank -> Maybe Rank
    rankCmd cmd r = case cmd of
      IFELSE p1 p2 -> let rank1 = rankP p1 r
                          rank2 = rankP p2 r
                     in case (rank1, rank2) of
                          (Just r1, Just r2) -> Just (min r1 r2)
                          _ -> Nothing
      _ -> let (n, m) = rankC cmd
           in if r >= n then Just (r - n + m) else Nothing

-- Function to evaluate a program with a given stack
run :: Prog -> Stack -> Result
run prog stack =
  case rankP prog (length stack) of
    Just r -> case semCmd prog stack of
                A newStack -> A newStack
                _ -> TypeError
    Nothing -> RankError

-- Auxiliary function to execute a single command
semCmd :: Prog -> Stack -> Result
semCmd [] stack = A stack
semCmd (cmd:cmds) stack = case cmd of
  IFELSE p1 p2 -> case stack of
    (B b : xs) ->
      if b
        then semCmd p1 xs
        else semCmd p2 xs
    _ -> RankError
  _ -> case execCmd cmd stack of
         Just newStack -> semCmd cmds newStack
         Nothing -> TypeError

-- Auxiliary function to execute a single command on the stack
execCmd :: Cmd -> Stack -> Maybe Stack
execCmd (LDI n) stack = Just (I n : stack)
execCmd ADD (I n1 : I n2 : stack) = Just (I (n1 + n2) : stack)
execCmd MULT (I n1 : I n2 : stack) = Just (I (n1 * n2) : stack)
execCmd DUP (val : stack) = Just (val : val : stack)
execCmd DEC (I n : stack) = Just (I (n - 1) : stack)
execCmd SWAP (val1 : val2 : stack) = Just (val2 : val1 : stack)
execCmd (POP n) stack = dropFromStack n stack
execCmd (IFELSE p1 p2) (B cond : stack)
  | cond = execCmdList p1 stack
  | otherwise = execCmdList p2 stack
execCmd (LDB b) stack = Just (B b : stack)
execCmd LEQ (I n1 : I n2 : stack) = Just (B (n1 <= n2) : stack)
execCmd _ _ = Nothing  -- Invalid stack configuration for the command

-- Auxiliary function to drop elements from the stack
dropFromStack :: Int -> Stack -> Maybe Stack
dropFromStack n stack
  | length stack >= n = Just (drop n stack)
  | otherwise = Nothing  -- Not enough elements on the stack

-- Auxiliary function to execute a list of commands
execCmdList :: Prog -> Stack -> Maybe Stack
execCmdList prog stack = foldl (\acc cmd -> acc >>= execCmd cmd) (Just stack) prog


