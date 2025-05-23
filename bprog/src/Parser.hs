
-- File: Parser.hs
--------------------------------------

{-# LANGUAGE RankNTypes #-} -- Forall keyword

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
processTokens [] [v]    = Success [v]
processTokens [] []     = Error [] "Empty stack"
processTokens [] stack  = Error stack "Stack has more than one value"

-- | Process valid tokens.
processTokens (token : tokens) stack = case token of
    
    -- Int operations.
    "+"       ->  continueWith (applyIntOp (+))             stack tokens
    "-"       ->  continueWith (applyIntOp (-))             stack tokens
    "*"       ->  continueWith (applyIntOp (*))             stack tokens
    "%"       ->  continueWith (applyIntOp mod)             stack tokens
    "div"     ->  continueWith (applyIntOp div)             stack tokens

    -- Float operations.        
    "/"       ->  continueWith (applyFloatOp (/))           stack tokens
    "pow"     ->  continueWith (applyFloatOp (**))          stack tokens

    -- Bool operations.
    "=="      ->  continueWith (applyOrdOp (==))            stack tokens
    "!="      ->  continueWith (applyOrdOp (/=))            stack tokens
    ">="      ->  continueWith (applyOrdOp (>=))            stack tokens
    "<="      ->  continueWith (applyOrdOp (<=))            stack tokens
    ">"       ->  continueWith (applyOrdOp (>))             stack tokens
    "<"       ->  continueWith (applyOrdOp (<))             stack tokens

    "or"      ->  continueWith  (applyBinaryBoolOp (||))    stack tokens
    "and"     ->  continueWith  (applyBinaryBoolOp (&&))    stack tokens
    "not"     ->  continueWith  (applyUnaryBoolOp not)      stack tokens

    -- Stack operations.
    "pop"     ->  continueWith applyPop                     stack tokens
    "drop"    ->  continueWith applyPop                     stack tokens
    "dup"     ->  continueWith applyDup                     stack tokens
    "swap"    ->  continueWith applySwap                    stack tokens
    "clear"   ->  continueWith applyClear                   stack tokens
    "print"   ->  continueWith applyPrint                   stack tokens
    "combine" ->  continueWith applyCombine                 stack tokens

    -- Default case for literals or unknown tokens.
    _ -> parseLiteralOrError token stack tokens

--------------------------------------
--          Int Operation           --
--------------------------------------

-- | Apply operation on integer.
--------------------------------------
applyIntOp :: (Int -> Int -> Int) -> Stack -> Result

-- Standard int operation.
applyIntOp operation ((VInt x) : (VInt y) : rest) = 
    Success (VInt (operation y x) : rest)

-- Error case.
applyIntOp _ s@[_] = Error s "Not enough operands"
applyIntOp _ [] = Error [] "Empty stack"
applyIntOp _ s = Error s "Invalid operand type"

--------------------------------------
--         Float Operation          --
--------------------------------------

-- | Apply operation on float.
--------------------------------------
applyFloatOp :: (Float -> Float -> Float) -> Stack -> Result

-- Standard float operation.
applyFloatOp operation ((VFloat x) : (VFloat y) : rest) = 
    Success (VFloat (operation y x) : rest)

-- Mixed int and float operation.
applyFloatOp operation ((VInt x) : (VFloat y) : rest) =
    Success (VFloat (operation y (fromIntegral x)) : rest)

-- Mixed float and int operation.
applyFloatOp operation ((VFloat x) : (VInt y) : rest) =
    Success (VFloat (operation x (fromIntegral y)) : rest)

-- Error case.
applyFloatOp _ s@[_] = Error s "Not enough operands"
applyFloatOp _ [] = Error [] "Empty stack"
applyFloatOp _ s = Error s "Invalid operand type"

--------------------------------------
--          Bool Operation          --
--------------------------------------

-- | Apply binary operation.
--------------------------------------
applyBinaryBoolOp :: (Bool -> Bool -> Bool) -> Stack -> Result

-- Standard operation.
applyBinaryBoolOp operation (VBool x : VBool y : rest) = 
    Success (VBool (operation y x) : rest)

-- Error case.
applyBinaryBoolOp _ s = Error s "Invalid operand type"

-- | Apply ordering operation.
--------------------------------------
applyOrdOp :: (forall a. Ord a => a -> a -> Bool) -> Stack -> Result

-- Standard operation.
applyOrdOp operation (VInt x : VInt y : rest)       = Success (VBool (operation y x) : rest)
applyOrdOp operation (VFloat x : VFloat y : rest)   = Success (VBool (operation y x) : rest)
applyOrdOp operation (VString x : VString y : rest) = Success (VBool (operation y x) : rest)

-- Mixed float/int operation.
applyOrdOp operation (VInt x : VFloat y : rest)     = Success (VBool (operation (fromIntegral x) y) : rest)
applyOrdOp operation (VFloat x : VInt y : rest)     = Success (VBool (operation x (fromIntegral y)) : rest)

-- Error case.
applyOrdOp _ s = Error s "Invalid operand type"

-- | Apply unary operation.
--------------------------------------
applyUnaryBoolOp :: (Bool -> Bool) -> Stack -> Result

-- Standard unary operation.
applyUnaryBoolOp operation (VBool x : rest) = 
    Success (VBool (operation x) : rest)

-- Error case.
applyUnaryBoolOp _ s = Error s "Invalid operand type"

--------------------------------------
--         Stack Operation          --
--------------------------------------

-- | Pop top element from stack.
--------------------------------------
applyPop :: Stack -> Result
applyPop (_ : rest) = Success rest
applyPop _ = Error [] "Empty stack"

-- | Duplicate top element of stack.
--------------------------------------
applyDup :: Stack -> Result
applyDup (x:xs) = Success (x:x:xs)
applyDup _ = Error [] "Empty stack"

-- | Clear stack.
--------------------------------------
applyClear :: Stack -> Result
applyClear _ = Success []

-- | Swap top two elements of stack.
--------------------------------------
applySwap :: Stack -> Result
applySwap (x:y:rest) = Success (y:x:rest)
applySwap [] = Error [] "Empty stack"
applySwap _ = Error [] "Not enough operands"

-- | Print current stack.
--------------------------------------
applyPrint :: Stack -> Result
applyPrint (x:xs) = Success (x:xs)
applyPrint _ = Error [] "Empty stack"

-- | Combine top two elements of stack.
--------------------------------------
applyCombine :: Stack -> Result

-- Standard string operation.
applyCombine (VString x : VString y : rest) =
    Success (VString (y ++ " " ++ x) : rest)

-- Standard int operation.
applyCombine (VInt x : VInt y : rest) =
    case readMaybe (show y ++ show x) :: Maybe Int of
        Just combined -> Success (VInt combined : rest)
        Nothing       -> Error (VInt x : VInt y : rest) "Invalid int combination"

-- Standard float operation.
applyCombine (VFloat x : VFloat y : rest) =
    case readMaybe (show y ++ show x) :: Maybe Float of
        Just combined -> Success (VFloat combined : rest)
        Nothing       -> Error (VFloat x : VFloat y : rest) "Invalid float combination"

-- Standard list operation.
applyCombine (VList x : VList y : rest) =
    Success (VList (y ++ x) : rest)

-- Standard dict operation.
applyCombine (VDict x : VDict y : rest) =
    Success (VDict (y ++ x) : rest)

-- Error case.
applyCombine s = Error s "Unsupported types for combine"

-- | Unary operation to top of stack.
--------------------------------------
continueWith :: (Stack -> Result) -> Stack -> [String] -> Result
continueWith f stack tokens = case f stack of
    Success s -> processTokens tokens s
    err       -> err

--------------------------------------
--         Default Operation        --
--------------------------------------

-- | Process a literal or error.
--------------------------------------
parseLiteralOrError :: String -> Stack -> [String] -> Result
parseLiteralOrError token stack tokens

    -- Parse list.
    | "[" == token =
        let (listTokens, rest) = extractUntilMatching "]" tokens
        in case parseList listTokens of
            Just val -> processTokens rest (push val stack)
            Nothing  -> Error stack "Invalid list literal"

    -- Parse quotation.
    | "{" == token =
        let (quoteTokens, rest) = extractUntilMatching "}" tokens
        in processTokens rest (push (VQuote quoteTokens) stack)

    -- Other literals.
    | Just n <- readMaybe token :: Maybe Int    = processTokens tokens (push (VInt n) stack)
    | Just f <- readMaybe token :: Maybe Float  = processTokens tokens (push (VFloat f) stack)
    | Just b <- readMaybe token :: Maybe Bool   = processTokens tokens (push (VBool b) stack)
    | Just s <- readMaybe token :: Maybe String = processTokens tokens (push (VString s) stack)

    -- Eror case.
    | otherwise = Error stack ("Unknown token: " ++ token)

-- | Extract until matching token.
--------------------------------------
extractUntilMatching :: String -> [String] -> ([String], [String])
extractUntilMatching closing = go (0 :: Int) []
    where
        opening = matchOpening closing
        go _ acc [] = (reverse acc, [])
        go n acc (t:ts)
            | t == closing && n == 0 = (reverse acc, ts)
            | t == closing           = go (n - 1) (t : acc) ts
            | t == opening           = go (n + 1) (t : acc) ts
            | otherwise              = go n (t : acc) ts

-- | Match opening token.
--------------------------------------
matchOpening :: String -> String
matchOpening "]" = "["
matchOpening "}" = "{"
matchOpening ")" = "("
matchOpening _   = error "Unsupported bracket type"

-- | Parse single value.
--------------------------------------
parseSingleValue :: String -> Maybe Value
parseSingleValue s =
    case readMaybe s :: Maybe Int of
        Just n -> Just (VInt n)
        Nothing -> case readMaybe s :: Maybe Float of
            Just f -> Just (VFloat f)
            Nothing -> case readMaybe s :: Maybe Bool of
                Just b -> Just (VBool b)
                Nothing -> case readMaybe s :: Maybe String of
                    Just str -> Just (VString str)
                    Nothing -> Nothing

-- | Parse list.
--------------------------------------
parseList :: [String] -> Maybe Value
parseList tokens =
    case parseListValues tokens [] of
        Just vs -> Just (VList vs)
        Nothing -> Nothing
    
-- | Parse list values.
--------------------------------------
parseListValues :: [String] -> [Value] -> Maybe [Value]
parseListValues [] acc = Just (reverse acc)
parseListValues ("[" : xs) acc =
    let (innerTokens, rest) = extractUntilMatching "]" xs
    in case parseList innerTokens of
        Just (VList inner) -> parseListValues rest (VList inner : acc)
        _ -> Nothing
parseListValues ("{" : xs) acc =
    let (quoteTokens, rest) = extractUntilMatching "}" xs
    in parseListValues rest (VQuote quoteTokens : acc)
parseListValues (t:ts) acc =
    case parseSingleValue t of
        Just v  -> parseListValues ts (v : acc)
        Nothing -> Nothing

--------------------------------------
--         String Operation         --
--------------------------------------
