{-# LANGUAGE RankNTypes #-} -- Forall keyword

--------------------------------------
--             Exports              --
--------------------------------------

module Parser (
    processTokens,
    processTokensLoose
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

-- | Process tokens and return new stack.
--------------------------------------
processTokens :: [String] -> Stack -> Result
processTokens [] [v]    = Success [v]
processTokens [] []     = Error [] "Empty stack"
processTokens [] stack  = Error stack "Stack has more than one value"

-- Process valid tokens.
processTokens (token : tokens) stack = case token of
    
    -- Int operations.
    "+"       ->  continueWith applyAdd                     stack tokens
    "-"       ->  continueWith applySub                     stack tokens
    "*"       ->  continueWith applyMul                     stack tokens
    "%"       ->  continueWith (applyIntOp mod)             stack tokens
    "div"     ->  continueWith (applyIntOp div)             stack tokens

    -- Float operations.        
    "/"       ->  continueWith (applyFloatOp (/))           stack tokens
    "pow"     ->  continueWith (applyFloatOp (**))          stack tokens

    -- Bool operations.
    "=="      ->  continueWith applyEqOp                    stack tokens
    "!="      ->  continueWith (applyOrdOp (/=))            stack tokens
    ">="      ->  continueWith (applyOrdOp (>=))            stack tokens
    "<="      ->  continueWith (applyOrdOp (<=))            stack tokens
    ">"       ->  continueWith (applyOrdOp (>))             stack tokens
    "<"       ->  continueWith (applyOrdOp (<))             stack tokens

    "||"      ->  continueWith  (applyBinaryBoolOp (||))    stack tokens
    "&&"      ->  continueWith  (applyBinaryBoolOp (&&))    stack tokens
    "not"     ->  continueWith  (applyUnaryBoolOp not)      stack tokens

    -- Stack operations.
    "pop"     ->  continueWith applyPop                     stack tokens
    "drop"    ->  continueWith applyPop                     stack tokens
    "dup"     ->  continueWith applyDup                     stack tokens
    "swap"    ->  continueWith applySwap                    stack tokens
    "clear"   ->  continueWith applyClear                   stack tokens
    "print"   ->  continueWith applyPrint                   stack tokens
    "combine" ->  continueWith applyCombine                 stack tokens
    
   -- List operations.
    "length"  ->  continueWith applyLength                  stack tokens
    "head"    ->  continueWith applyHead                    stack tokens
    "tail"    ->  continueWith applyTail                    stack tokens
    "empty"   ->  continueWith applyEmpty                   stack tokens
    "cons"    ->  continueWith applyCons                    stack tokens
    "append"  ->  continueWith applyAppend                  stack tokens

    -- Control flow.
    "times"   ->  continueWith applyTimes                   stack tokens
    "loop"    ->  continueWith applyLoop                    stack tokens
    "if"      ->  applyIfSpecial                            stack tokens

    -- Functions.
    "parseInteger" -> continueWith applyParseInteger        stack tokens
    "parseFloat"   -> continueWith applyParseFloat          stack tokens
    "words"        -> continueWith applyWords               stack tokens
    "map"          -> applyMapSpecial                       stack tokens
    "each"         -> applyEachSpecial                      stack tokens
    "foldl"        -> applyFoldlSpecial                     stack tokens
    "fun"          -> continueWith applyFun                 stack tokens
    ":="           -> continueWith applyAssign              stack tokens

    -- Execution control.
    "exec"    ->  continueWith applyExec                    stack tokens

    -- Default case for literals or unknown tokens.
    _ -> parseLiteralOrError token stack tokens

-- | Process tokens and return new stack (Looser version for scripts).
--------------------------------------
processTokensLoose :: [String] -> Stack -> Result
processTokensLoose [] stack = Success stack

-- Process valid tokens.
processTokensLoose (token : tokens) stack = case token of

    -- Int operations.
    "+"       ->  continueWith applyAdd                     stack tokens
    "-"       ->  continueWith applySub                     stack tokens
    "*"       ->  continueWith applyMul                     stack tokens
    "%"       ->  continueWith (applyIntOp mod)             stack tokens
    "div"     ->  continueWith (applyIntOp div)             stack tokens

    -- Float operations.        
    "/"       ->  continueWith (applyFloatOp (/))           stack tokens
    "pow"     ->  continueWith (applyFloatOp (**))          stack tokens

    -- Bool operations.
    "=="      ->  continueWith applyEqOp                    stack tokens
    "!="      ->  continueWith (applyOrdOp (/=))            stack tokens
    ">="      ->  continueWith (applyOrdOp (>=))            stack tokens
    "<="      ->  continueWith (applyOrdOp (<=))            stack tokens
    ">"       ->  continueWith (applyOrdOp (>))             stack tokens
    "<"       ->  continueWith (applyOrdOp (<))             stack tokens

    "||"      ->  continueWith  (applyBinaryBoolOp (||))    stack tokens
    "&&"      ->  continueWith  (applyBinaryBoolOp (&&))    stack tokens
    "not"     ->  continueWith  (applyUnaryBoolOp not)      stack tokens

    -- Stack operations.
    "pop"     ->  continueWith applyPop                     stack tokens
    "drop"    ->  continueWith applyPop                     stack tokens
    "dup"     ->  continueWith applyDup                     stack tokens
    "swap"    ->  continueWith applySwap                    stack tokens
    "clear"   ->  continueWith applyClear                   stack tokens
    "print"   ->  continueWith applyPrint                   stack tokens
    "combine" ->  continueWith applyCombine                 stack tokens
    
    -- List operations.
    "length"  ->  continueWith applyLength                  stack tokens
    "head"    ->  continueWith applyHead                    stack tokens
    "tail"    ->  continueWith applyTail                    stack tokens
    "empty"   ->  continueWith applyEmpty                   stack tokens
    "cons"    ->  continueWith applyCons                    stack tokens
    "append"  ->  continueWith applyAppend                  stack tokens

    -- Control flow.
    "times"   ->  continueWithLoose applyTimes              stack tokens
    "loop"    ->  continueWithLoose applyLoop               stack tokens
    "if"      ->  applyIfSpecialLoose                       stack tokens

    -- Functions.
    "parseInteger" -> continueWith applyParseInteger        stack tokens
    "parseFloat"   -> continueWith applyParseFloat          stack tokens
    "words"        -> continueWith applyWords               stack tokens
    "map"          -> applyMapSpecial                       stack tokens
    "each"         -> applyEachSpecial                      stack tokens  
    "foldl"        -> applyFoldlSpecial                     stack tokens
    "fun"          -> continueWith applyFun                 stack tokens
    ":="           -> continueWith applyAssign              stack tokens

    -- Execution control.
    "exec"    ->  continueWithLoose applyExec               stack tokens

    -- Default case for literals or unknown tokens.
    _ -> parseLiteralOrErrorLoose token stack tokens

--------------------------------------
--          Int Operation           --
--------------------------------------

-- | Apply operation on integer.
--------------------------------------
applyIntOp :: (Integer -> Integer -> Integer) -> Stack -> Result

-- Standard int operation.
applyIntOp operation ((VInt x) : (VInt y) : rest) = 
    Success (VInt (operation y x) : rest)

-- Mixed int and float operation.
applyIntOp operation ((VInt x) : (VFloat y) : rest) =
    Success (VInt (truncate y `operation` x) : rest)
applyIntOp operation ((VFloat x) : (VInt y) : rest) =
    Success (VInt (y `operation` truncate x) : rest)

-- Float to int operation.
applyIntOp operation ((VFloat x) : (VFloat y) : rest) =
    Success (VInt (truncate y `operation` truncate x) : rest)

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
    Success (VFloat (operation (fromIntegral y) x) : rest)

-- Int to float operation.
applyFloatOp operation (VInt x : VInt y : rest) =
    Success (VFloat (fromIntegral y `operation` fromIntegral x) : rest)

-- Error case.
applyFloatOp _ s@[_] = Error s "Not enough operands"
applyFloatOp _ [] = Error [] "Empty stack"
applyFloatOp _ s = Error s "Invalid operand type"

--------------------------------------
--         Mixed Operation          --
--------------------------------------

-- | Apply + for int and float types.
--------------------------------------
applyAdd :: Stack -> Result
applyAdd (VInt x : VInt y : rest) = Success (VInt (y + x) : rest)
applyAdd (VFloat x : VFloat y : rest) = Success (VFloat (y + x) : rest)
applyAdd (VInt x : VFloat y : rest) = Success (VFloat (y + fromIntegral x) : rest)
applyAdd (VFloat x : VInt y : rest) = Success (VFloat (fromIntegral y + x) : rest)
applyAdd s = Error s "Invalid operand type for +"

-- | Apply - for int and float types.
--------------------------------------
applySub :: Stack -> Result
applySub (VInt x : VInt y : rest) = Success (VInt (y - x) : rest)
applySub (VFloat x : VFloat y : rest) = Success (VFloat (y - x) : rest)
applySub (VInt x : VFloat y : rest) = Success (VFloat (y - fromIntegral x) : rest)
applySub (VFloat x : VInt y : rest) = Success (VFloat (fromIntegral y - x) : rest)
applySub s = Error s "Invalid operand type for -"

-- | Apply * for int and float types.
--------------------------------------
applyMul :: Stack -> Result
applyMul (VInt x : VInt y : rest) = Success (VInt (y * x) : rest)
applyMul (VFloat x : VFloat y : rest) = Success (VFloat (y * x) : rest)
applyMul (VInt x : VFloat y : rest) = Success (VFloat (y * fromIntegral x) : rest)
applyMul (VFloat x : VInt y : rest) = Success (VFloat (fromIntegral y * x) : rest)
applyMul s = Error s "Invalid operand type for *"

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
applyOrdOp operation (VInt x : VInt y : rest) = 
    Success (VBool (operation y x) : rest)
applyOrdOp operation (VFloat x : VFloat y : rest) = 
    Success (VBool (operation y x) : rest)
applyOrdOp operation (VString x : VString y : rest) = 
    Success (VBool (operation y x) : rest)

-- Mixed float/int operation.
applyOrdOp operation (VInt x : VFloat y : rest) =
    Success (VBool (y `operation` fromIntegral x) : rest)
applyOrdOp operation (VFloat x : VInt y : rest) =
    Success (VBool (fromIntegral y `operation` x) : rest)

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

-- | Apply equality operation.
--------------------------------------
applyEqOp :: Stack -> Result

-- Mixed int/float operation.
applyEqOp (VInt x : VFloat y : rest) =
    Success (VBool (fromIntegral x == y) : rest)
applyEqOp (VFloat x : VInt y : rest) =
    Success (VBool (x == fromIntegral y) : rest)

-- Standard equality operation.
applyEqOp (x : y : rest)
    | sameType x y = Success (VBool (x == y) : rest)
    | otherwise    = Error (x : y : rest) "Invalid operand type"

-- Error case.
applyEqOp s = Error s "Invalid operand type"

-- | Ensure only equal types can be compared.
--------------------------------------
sameType :: Value -> Value -> Bool
sameType (VInt _)     (VInt _)     = True
sameType (VFloat _)   (VFloat _)   = True
sameType (VString _)  (VString _)  = True
sameType (VBool _)    (VBool _)    = True
sameType (VList xs)   (VList ys)   = length xs == length ys && all (uncurry sameType) (zip xs ys)
sameType (VQuote _)   (VQuote _)   = True
sameType (VDict _)    (VDict _)    = True
sameType _            _            = False

--------------------------------------
--         Stack Operation          --
--------------------------------------

-- | Apply 'pop' operation.
--------------------------------------
applyPop :: Stack -> Result
applyPop (_ : rest) = Success rest
applyPop _ = Error [] "Empty stack"

-- | Apply 'dup' operation.
--------------------------------------
applyDup :: Stack -> Result
applyDup (x:xs) = Success (x:x:xs)
applyDup _ = Error [] "Empty stack"

-- | Apply 'clear' operation.
--------------------------------------
applyClear :: Stack -> Result
applyClear _ = Success []

-- | Apply 'swap' operation.
--------------------------------------
applySwap :: Stack -> Result
applySwap (x:y:rest) = Success (y:x:rest)
applySwap [] = Error [] "Empty stack"
applySwap _ = Error [] "Not enough operands"

-- | Apply 'print' operation.
--------------------------------------
applyPrint :: Stack -> Result
applyPrint (x:xs) = Info xs (show x)
applyPrint _ = Error [] "Empty stack"

-- | Apply 'combine' operation.
--------------------------------------
applyCombine :: Stack -> Result

-- Standard string operation.
applyCombine (VString x : VString y : rest) =
    Success (VString (y ++ " " ++ x) : rest)

-- Standard int operation.
applyCombine (VInt x : VInt y : rest) =
    case readMaybe (show y ++ show x) :: Maybe Integer of
        Just combined -> Success (VInt combined : rest)
        Nothing       -> Error (VInt x : VInt y : rest) "Invalid int combination"

-- Standard float operation.
applyCombine (VFloat x : VFloat y : rest) =
    Success (VFloat (y + x) : rest)

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

-- | Unary operation to top of stack (loose version).
--------------------------------------
continueWithLoose :: (Stack -> Result) -> Stack -> [String] -> Result
continueWithLoose f stack tokens = case f stack of
    Success s -> processTokensLoose tokens s
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
            Nothing  -> Error stack ("Invalid list literal" ++ " " ++ show listTokens)

    -- Parse quotation.
    | "{" == token =
        let (quoteTokens, rest) = extractUntilMatching "}" tokens
        in processTokens rest (VQuote quoteTokens : stack)

    -- Other literals.
    | Just n <- readMaybe token :: Maybe Integer    = processTokens tokens (push (VInt n) stack)
    | Just f <- readMaybe token :: Maybe Float      = processTokens tokens (push (VFloat f) stack)
    | Just b <- readMaybe token :: Maybe Bool       = processTokens tokens (push (VBool b) stack)

    -- String
    | isQuoted token = processTokens tokens (push (VString (stripQuotes token)) stack)

    -- Error case.
    | otherwise = Error stack ("Unknown token: " ++ token)

-- | Process a literal or error (loose version).
--------------------------------------
parseLiteralOrErrorLoose :: String -> Stack -> [String] -> Result
parseLiteralOrErrorLoose token stack tokens

    -- Parse list.
    | "[" == token =
        let (listTokens, rest) = extractUntilMatching "]" tokens
        in case parseList listTokens of
            Just val -> processTokensLoose rest (push val stack)
            Nothing  -> Error stack "Invalid list literal"

    -- Parse quotation.
    | "{" == token =
        let (quoteTokens, rest) = extractUntilMatching "}" tokens
        in processTokensLoose rest (VQuote quoteTokens : stack)

    -- Other literals.
    | Just n <- readMaybe token :: Maybe Integer    = processTokensLoose tokens (push (VInt n) stack)
    | Just f <- readMaybe token :: Maybe Float      = processTokensLoose tokens (push (VFloat f) stack)
    | Just b <- readMaybe token :: Maybe Bool       = processTokensLoose tokens (push (VBool b) stack)

    -- String
    | isQuoted token = processTokensLoose tokens (push (VString (stripQuotes token)) stack)

    -- Variables (for loose mode)
    | otherwise = processTokensLoose tokens (VString token : stack)

-- | Parse a literal token.
--------------------------------------  
parseLiteral :: String -> Maybe Value
parseLiteral token
    | Just n <- readMaybe token :: Maybe Integer = Just (VInt n)
    | Just f <- readMaybe token :: Maybe Float = Just (VFloat f) 
    | Just b <- readMaybe token :: Maybe Bool = Just (VBool b)
    | isQuoted token = Just (VString (stripQuotes token))
    | otherwise = Nothing

-- | Extract until matching token.
--------------------------------------
extractUntilMatching :: String -> [String] -> ([String], [String])
extractUntilMatching closing = go (0 :: Integer) []
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
parseSingleValue s
    | isQuoted s = Just (VString (stripQuotes s))
    | otherwise = 
        case readMaybe s :: Maybe Integer of
            Just n -> Just (VInt n)
            Nothing -> case readMaybe s :: Maybe Float of
                Just f -> Just (VFloat f)
                Nothing -> case readMaybe s :: Maybe Bool of
                    Just b -> Just (VBool b)
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
        Just innerList -> parseListValues rest (innerList : acc)
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

-- | Check if a string is quoted.
--------------------------------------
isQuoted :: String -> Bool
isQuoted s = case s of
    ('"' : xs) -> not (null xs) && last xs == '"'
    _ -> False

-- | Strip quotes from a string.
--------------------------------------
stripQuotes :: String -> String
stripQuotes = init . tail

--------------------------------------
--          List Operation          --
--------------------------------------

-- | Apply 'length' operation.
--------------------------------------
applyLength :: Stack -> Result
applyLength (VList xs : rest) = 
    Success (VInt (fromIntegral $ length xs) : rest)
applyLength (VString s : rest) = 
    Success (VInt (fromIntegral $ length s) : rest)
applyLength (VQuote q : rest) = 
    Success (VInt (fromIntegral $ length q) : rest)
applyLength s = Error s "Unsupported type for length"

-- | Apply 'head' operation.
--------------------------------------
applyHead :: Stack -> Result
applyHead (VList (x:_) : rest) = 
    Success (x : rest)
applyHead s = Error s "Expected non-empty list for head"

-- | Apply 'tail' operation.
--------------------------------------
applyTail :: Stack -> Result
applyTail (VList (_:xs) : rest) = 
    Success (VList xs : rest)
applyTail s = Error s "Expected non-empty list for tail"

-- | Apply 'empty' operation.
--------------------------------------
applyEmpty :: Stack -> Result
applyEmpty (VList xs : rest) = 
    Success (VBool (null xs) : rest)
applyEmpty s = Error s "Expected a list for empty"

-- | Apply 'cons' operation.
--------------------------------------
applyCons :: Stack -> Result
applyCons (VList xs : item : rest) = 
    Success (VList (item : xs) : rest)
applyCons s = Error s "Invalid operands for cons"

-- | Apply 'append' operation.
--------------------------------------
applyAppend :: Stack -> Result
applyAppend (VList a : VList b : rest) = 
    Success (VList (b ++ a) : rest)
applyAppend s = Error s "Invalid operands for append"

--------------------------------------
--           Control flow           --
--------------------------------------

-- | Apply 'times' operation.
--------------------------------------
applyTimes :: Stack -> Result

applyTimes (VInt n : VQuote q : rest)
    | n >= 0 = processTokensLoose (concat (replicate (fromIntegral n) q)) rest
    | otherwise = Error (VInt n : VQuote q : rest) "Negative repeat count"

applyTimes (VQuote q : VInt n : rest)
    | n >= 0 = processTokensLoose (concat (replicate (fromIntegral n) q)) rest
    | otherwise = Error (VQuote q : VInt n : rest) "Negative repeat count"

applyTimes (x : VInt n : rest)
    | n >= 0 = Success (replicate (fromIntegral n) x ++ rest)
    | otherwise = Error (x : VInt n : rest) "Negative repeat count"

-- Error case.
applyTimes s = Error s "Invalid operands for times"

-- | Apply 'loop' operation.
--------------------------------------
applyLoop :: Stack -> Result
applyLoop (VQuote body : VQuote cond : rest) = runLoop cond body rest
applyLoop s = Error s "Invalid operands for loop"

-- | Loop evaluation.
--------------------------------------
runLoop :: [String] -> [String] -> Stack -> Result
runLoop cond body stack =
    case processTokensLoose cond stack of
        Success (VBool True : rest') ->
            case processTokensLoose body rest' of
                Success stack' -> runLoop cond body stack'
                err            -> err
        Success (VBool False : rest') -> Success rest'
        Success _                     -> Error stack "Condition must produce Bool"
        Error _ msg                   -> Error stack msg
        Warning _ msg                 -> Error stack ("Unexpected warning: " ++ msg)
        Info _ msg                    -> Error stack ("Unexpected info: " ++ msg)

-- | Apply 'if' operation with special token handling.
--------------------------------------
applyIfSpecial :: Stack -> [String] -> Result
applyIfSpecial (VBool cond : rest) tokens =
    case tokens of

        -- Handle { then-block } { else-block }
        ("{" : thenStart) ->
            let (thenBody, afterThen) = extractUntilMatching "}" thenStart
            in case afterThen of
                ("{" : elseStart) ->
                    let (elseBody, remaining) = extractUntilMatching "}" elseStart
                        chosenBody = if cond then thenBody else elseBody
                    in processTokens (chosenBody ++ remaining) rest
                -- Handle { then-block } else-value
                (elseToken : remaining) ->
                    if cond
                        then processTokens (thenBody ++ remaining) rest
                        else case parseLiteral elseToken of
                            Just val -> processTokens remaining (val : rest)
                            Nothing -> Error (VBool cond : rest) ("Cannot parse else value: " ++ elseToken)
                [] -> Error (VBool cond : rest) "Missing else part"
        
        -- Handle then-value { else-block }
        (thenToken : "{" : elseStart) ->
            let (elseBody, remaining) = extractUntilMatching "}" elseStart
            in if cond
                then case parseLiteral thenToken of
                    Just val -> processTokens remaining (val : rest)
                    Nothing -> Error (VBool cond : rest) ("Cannot parse then value: " ++ thenToken)
                else processTokens (elseBody ++ remaining) rest

        -- Handle then-value else-value
        (thenToken : elseToken : remaining) ->
            case (parseLiteral thenToken, parseLiteral elseToken) of
                (Just thenVal, Just elseVal) ->
                    let chosenVal = if cond then thenVal else elseVal
                    in processTokens remaining (chosenVal : rest)
                _ -> Error (VBool cond : rest) "Cannot parse if values"
        _ -> Error (VBool cond : rest) "Insufficient tokens for if"
applyIfSpecial stack _ = Error stack "Expected boolean for if condition"

-- | Apply 'if' operation with special token handling (loose version).
--------------------------------------
applyIfSpecialLoose :: Stack -> [String] -> Result  
applyIfSpecialLoose (VBool cond : rest) tokens =
    case tokens of

        -- Handle { then-block } { else-block }
        ("{" : thenStart) ->
            let (thenBody, afterThen) = extractUntilMatching "}" thenStart
            in case afterThen of
                ("{" : elseStart) ->
                    let (elseBody, remaining) = extractUntilMatching "}" elseStart
                        chosenBody = if cond then thenBody else elseBody
                    in processTokensLoose (chosenBody ++ remaining) rest

                -- Handle { then-block } else-value  
                (elseToken : remaining) ->
                    if cond
                        then processTokensLoose (thenBody ++ remaining) rest
                        else case parseLiteral elseToken of
                            Just val -> processTokensLoose remaining (val : rest)
                            Nothing -> processTokensLoose remaining (VString elseToken : rest)
                [] -> Error (VBool cond : rest) "Missing else part"

        -- Handle then-value { else-block }
        (thenToken : "{" : elseStart) ->
            let (elseBody, remaining) = extractUntilMatching "}" elseStart
            in if cond
                then case parseLiteral thenToken of
                    Just val -> processTokensLoose remaining (val : rest)
                    Nothing -> processTokensLoose remaining (VString thenToken : rest)
                else processTokensLoose (elseBody ++ remaining) rest

        -- Handle then-value else-value
        (thenToken : elseToken : remaining) ->
            let thenVal = case parseLiteral thenToken of
                    Just v -> v
                    Nothing -> VString thenToken
                elseVal = case parseLiteral elseToken of  
                    Just v -> v
                    Nothing -> VString elseToken
                chosenVal = if cond then thenVal else elseVal
            in processTokensLoose remaining (chosenVal : rest)
        _ -> Error (VBool cond : rest) "Insufficient tokens for if"
applyIfSpecialLoose stack _ = Error stack "Expected boolean for if condition"

--------------------------------------
--        Function Operation        --
--------------------------------------

-- | Apply 'parseInteger' operation.
--------------------------------------
applyParseInteger :: Stack -> Result
applyParseInteger (VString s : rest) = 
    case readMaybe s :: Maybe Integer of
        Just n -> Success (VInt n : rest)
        Nothing -> Error (VString s : rest) "Invalid integer string"
applyParseInteger s = Error s "Expected string for parseInteger"

-- | Apply 'parseFloat' operation.
--------------------------------------
applyParseFloat :: Stack -> Result
applyParseFloat (VString s : rest) = 
    case readMaybe s :: Maybe Float of
        Just f -> Success (VFloat f : rest)
        Nothing -> Error (VString s : rest) "Invalid float string"
applyParseFloat s = Error s "Expected string for parseFloat"

-- | Apply 'words' operation.
--------------------------------------
applyWords :: Stack -> Result
applyWords (VString s : rest) = 
    Success (VList (map VString (words s)) : rest)
applyWords s = Error s "Expected string for words"

-- | Apply 'map' operation.
--------------------------------------
applyMapSpecial :: Stack -> [String] -> Result
applyMapSpecial (VList xs : rest) tokens =
    case tokens of
        ("{" : quoteStart) ->
            let (quoteBody, remaining) = extractUntilMatching "}" quoteStart
            in case mapList quoteBody xs of
                Right mapped -> processTokensLoose remaining (VList mapped : rest)
                Left err -> Error (VList xs : rest) err
        _ -> Error (VList xs : rest) "Expected quotation after map"
applyMapSpecial stack _ = Error stack "Expected list for map"

-- Helper function to map over a list
mapList :: [String] -> [Value] -> Either String [Value]
mapList _ [] = Right []
mapList quote (x:xs) = 
    case applyQuotationToItem quote x of
        Right result -> case mapList quote xs of
            Right rest -> Right (result : rest)
            Left err -> Left err
        Left err -> Left err

-- | Apply quotation to item.
applyQuotationToItem :: [String] -> Value -> Either String Value
applyQuotationToItem quote val = 
    case processTokensLoose quote [val] of
        Success [result] -> Right result
        Success [] -> Left "Empty result from quotation"
        Success (result:_) -> Right result
        Error _ msg -> Left msg
        _ -> Left "Unexpected result type"

-- | Apply 'each' operation.
--------------------------------------
applyEachSpecial :: Stack -> [String] -> Result
applyEachSpecial (VList xs : rest) tokens =
    case tokens of
        ("{" : quoteStart) ->
            let (quoteBody, remaining) = extractUntilMatching "}" quoteStart
            in case processEach quoteBody xs [] of
                Right results -> processTokensLoose remaining (reverse results ++ rest)
                Left err -> Error (VList xs : rest) err

        (op : remaining) ->
            let quote = [op] in
            case processEach quote xs [] of
                Right results -> processTokensLoose remaining (reverse results ++ rest)
                Left err -> Error (VList xs : rest) err

        _ -> Error (VList xs : rest) "Expected quotation or operator after each"
applyEachSpecial stack _ = Error stack "Expected list for each"


-- | Apply 'foldl' operation. 
--------------------------------------
applyFoldlSpecial :: Stack -> [String] -> Result
applyFoldlSpecial (acc : VList xs : rest) tokens =
    case tokens of
        ("{" : quoteStart) ->
            let (quoteBody, remaining) = extractUntilMatching "}" quoteStart
            in case foldList quoteBody acc xs of
                Right result -> processTokensLoose remaining (result : rest)
                Left err -> Error (acc : VList xs : rest) err

        -- Handle non-quotation operators (like + or div)
        (op : remaining) ->
            case foldlOp (VString op) acc xs of
                Right result -> processTokensLoose remaining (result : rest)
                Left err -> Error (acc : VList xs : rest) err
        _ -> Error (acc : VList xs : rest) "Expected quotation or operator after foldl"
applyFoldlSpecial stack _ = Error stack "Expected accumulator and list for foldl"

foldlOp :: Value -> Value -> [Value] -> Either String Value
foldlOp (VString op) acc xs = foldlOpHelper op acc xs
foldlOp _ _ _ = Left "Invalid operator for foldl"

foldlOpHelper :: String -> Value -> [Value] -> Either String Value
foldlOpHelper _ acc [] = Right acc
foldlOpHelper op acc (x:xs) =
    case processTokensLoose [op] [x, acc] of
        Success [result] -> foldlOpHelper op result xs
        Error _ msg -> Left msg
        _ -> Left "Unexpected result"

-- | Apply 'assign' operation.
--------------------------------------
applyAssign :: Stack -> Result
applyAssign (value : VString _varName : rest) =
    Success (value : rest)
applyAssign s = Error s "Invalid operands for :="

-- | Apply 'fun' operation.
--------------------------------------
applyFun :: Stack -> Result
applyFun (VQuote _body : VString _name : rest) =
    Success rest
applyFun s = Error s "Invalid operands for fun"

-- | Process each item in the list.
--------------------------------------
processEach :: [String] -> [Value] -> [Value] -> Either String [Value]
processEach _ [] acc = Right acc
processEach quote (x:xs) acc =
    case processTokensLoose quote [x] of
        Success [result] -> processEach quote xs (acc ++ [result])
        Error _ msg      -> Left msg
        _                -> Left "Unexpected result"

-- | Apply list fold.
--------------------------------------
foldList :: [String] -> Value -> [Value] -> Either String Value
foldList _ acc [] = Right acc
foldList quote acc (x:xs) =
    case processTokensLoose quote [x, acc] of
        Success [result] -> foldList quote result xs
        Error _ msg -> Left msg
        _ -> Left "Unexpected result"

--------------------------------------
--             Execution            --
--------------------------------------

-- | Apply execution control.
--------------------------------------
applyExec :: Stack -> Result
applyExec (VQuote q : rest) = processTokensLoose q rest
applyExec (x : xs) = Error (x : xs) "Expected a quotation on top of the stack"
applyExec [] = Error [] "Empty stack"