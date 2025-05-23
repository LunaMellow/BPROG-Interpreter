
-- File: Types.hs
--------------------------------------

--------------------------------------
--             Exports              --
--------------------------------------

module Types 
( 
    Value(..),
    Result(..),
    Stack, 
    push, 
    pop, 
    asInt, 
    asString, 
    asBool, 
    asFloat, 
    asList, 
    asDict, 
    asFunction
) where

--------------------------------------
--              VALUE               --
--------------------------------------

-- | The Value datatype represents values that can be pushed to stack.
data Value = 
    VInt            Int
    | VString       String
    | VBool         Bool
    | VFloat        Float
    | VQuote        [String]
    | VList         [Value]
    | VDict         [(String, Value)]
    | VFunction     ([Value] -> Value)

-- | The Show instance for the Value datatype.
instance Show Value where
    show (VInt i)       = show i
    show (VString s)    = show s
    show (VBool b)      = show b
    show (VFloat f)     = show f
    show (VQuote q)     = show q
    show (VList l)      = show l
    show (VDict d)      = show d
    show (VFunction _)  = "<fn>"

-- | The equality instance for the Value datatype.
instance Eq Value where
    (VInt a)    == (VInt b)     = a == b
    (VString a) == (VString b)  = a == b
    (VBool a)   == (VBool b)    = a == b
    (VFloat a)  == (VFloat b)   = a == b
    (VQuote a)  == (VQuote b)   = a == b
    (VList a)   == (VList b)    = a == b
    (VDict a)   == (VDict b)    = a == b
    _ == _ = False

-- | Extract a int from the Value datatype.
asInt :: Value -> Maybe Int
asInt (VInt i) = Just i
asInt _ = Nothing

-- | Extract a string from the Value datatype.
asString :: Value -> Maybe String
asString (VString s) = Just s
asString _ = Nothing

-- | Extract a bool from the Value datatype.
asBool :: Value -> Maybe Bool
asBool (VBool b) = Just b
asBool _ = Nothing

-- | Extract a float from the Value datatype.
asFloat :: Value -> Maybe Float
asFloat (VFloat f) = Just f
asFloat _ = Nothing

-- | Extract a list from the Value datatype.
asList :: Value -> Maybe [Value]
asList (VList l) = Just l
asList _ = Nothing

-- | Extract a dict from the Value datatype.
asDict :: Value -> Maybe [(String, Value)]
asDict (VDict d) = Just d
asDict _ = Nothing

-- | Extract a function from the Value datatype.
asFunction :: Value -> Maybe ([Value] -> Value)
asFunction (VFunction f) = Just f
asFunction _ = Nothing

--------------------------------------
--              STACK               --
--------------------------------------

-- | The Stack datatype represents a stack of values.
type Stack = [Value]

-- | Push a value onto the stack.
push :: Value -> Stack -> Stack
push x xs = x : xs

-- | Pop a value from the stack.
pop :: Stack -> Maybe (Value, Stack)
pop [] = Nothing
pop (x:xs) = Just (x, xs)

--------------------------------------
--              RESULT              --
--------------------------------------

-- | The Result datatype represents the result of processing tokens.
data Result =
    Success Stack
    | Error Stack String
    | Warning Stack String
    | Info Stack String
