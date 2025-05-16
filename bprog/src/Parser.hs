
-- File: Parser.hs
--------------------------------------

--------------------------------------
--             Exports              --
--------------------------------------

module Parser (
    processTokens
) where

--------------------------------------
--             Imports              --
--------------------------------------

-- Local.

import Types (
    Value(..), 
    Result(..),
    Stack, 
    push
    )

-- External.

import Text.Read (
    readMaybe
    )

--------------------------------------
--          Process Tokens          --
--------------------------------------

-- | Process a list of tokens and return a new stack.
processTokens :: [String] -> Stack -> Result
processTokens [] []     = Error [] "Empty stack"
processTokens [] [v]    = Success [v]
processTokens [] stack  = Error stack "Stack has more than one value"

-- | Process valid tokens.
processTokens (token : tokens) stack = case token of
    
    -- Int operations.
    "+"     ->  continueWith (applyIntOp (+)) stack tokens
    "-"     ->  continueWith (applyIntOp (-)) stack tokens
    "*"     ->  continueWith (applyIntOp (*)) stack tokens
    "%"     ->  continueWith (applyIntOp mod) stack tokens
    "div"   ->  continueWith (applyIntOp div) stack tokens

    -- Float operations.
    "/"     ->  continueWith (applyFloatOp (/)) stack tokens
    "pow"   ->  continueWith (applyFloatOp (**)) stack tokens

    -- Stack operations.
    "pop"   ->  continueWith applyPop stack tokens

    -- Default case for literals or unknown tokens.
    _ -> parseLiteralOrError token stack tokens

--------------------------------------
--          Int Operation           --
--------------------------------------

-- | Apply operation on integer.
applyIntOp :: (Int -> Int -> Int) -> Stack -> Result
applyIntOp _ s@[_] = Error s "Not enough operands"
applyIntOp _ [] = Error [] "Empty stack"

-- Standard int operation.
applyIntOp operation ((VInt x) : (VInt y) : rest) = 
    Success (VInt (operation y x) : rest)

-- Invalid operand type.
applyIntOp _ s = Error s "Invalid operand type"

--------------------------------------
--         Float Operation          --
--------------------------------------

-- | Apply operation on float.
applyFloatOp :: (Float -> Float -> Float) -> Stack -> Result
applyFloatOp _ s@[_] = Error s "Not enough operands"
applyFloatOp _ [] = Error [] "Empty stack"

-- Standard float operation.
applyFloatOp operation ((VFloat x) : (VFloat y) : rest) = 
    Success (VFloat (operation y x) : rest)

-- Miexed int and float operation.
applyFloatOp operation ((VInt x) : (VFloat y) : rest) =
    Success (VFloat (operation y (fromIntegral x)) : rest)

-- Mixed float and int operation.
applyFloatOp operation ((VFloat x) : (VInt y) : rest) =
    Success (VFloat (operation x (fromIntegral y)) : rest)

-- Invalid operand type.
applyFloatOp _ s = Error s "Invalid operand type"

--------------------------------------
--         Stack Operation          --
--------------------------------------

-- | Pop the top element from the stack.
applyPop :: Stack -> Result
applyPop (_ : rest) = Success rest
applyPop _ = Error [] "Empty stack"

-- | Apply a unary operation to the top element of the stack.
continueWith :: (Stack -> Result) -> Stack -> [String] -> Result
continueWith f stack tokens = case f stack of
    Success s -> processTokens tokens s
    err       -> err

--------------------------------------
--         Default Operation        --
--------------------------------------

-- | Process a literal or error.
parseLiteralOrError :: String -> Stack -> [String] -> Result
parseLiteralOrError token stack tokens
  | Just n <- readMaybe token :: Maybe Int    = processTokens tokens (push (VInt n) stack)
  | Just f <- readMaybe token :: Maybe Float  = processTokens tokens (push (VFloat f) stack)
  | Just b <- readMaybe token :: Maybe Bool   = processTokens tokens (push (VBool b) stack)
  | Just s <- readMaybe token :: Maybe String = processTokens tokens (push (VString s) stack)
  | otherwise                                 = Error stack ("Unknown token: " ++ token)

--------------------------------------
--         String Operation         --
--------------------------------------
