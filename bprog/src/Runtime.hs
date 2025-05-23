
-- File: Runtime.hs
--------------------------------------

--------------------------------------
--             Exports              --
--------------------------------------

module Runtime ( 
    handleResult,
    runFile,
    mergeQuotedStrings,
    stripComments
) where

--------------------------------------
--             Imports              --
--------------------------------------

-- Local.

import Types (
    Result(..),
    Value(..),
    Stack
    )

import Parser (
    processTokensLoose
    )

--------------------------------------
--             Runtime              --
--------------------------------------

-- | Runs a file and processes its content.
--------------------------------------
runFile :: FilePath -> IO ()
runFile path = do
    content <- readFile path
    let lines' = lines content
    runLines [] lines'

-- | Processes each line of the file.
--------------------------------------
runLines :: Stack -> [String] -> IO ()
runLines _ [] = return ()
runLines stack (l:ls) = do
    let cleaned = stripComments l
    let tokens = mergeQuotedStrings (words cleaned)
    handleResult (\newStack -> runLines newStack ls) (processTokensLoose tokens stack)

--------------------------------------
--            Functions             --
--------------------------------------

-- | Pretty print the stack.
prettyPrint :: [Value] -> String
prettyPrint [v] = show v
prettyPrint vs = show vs

-- | Merge quoted strings.
--------------------------------------
mergeQuotedStrings :: [String] -> [String]
mergeQuotedStrings = go False []
    where
        go _ acc [] = reverse acc
        go False acc (t:ts)
            | head t == '"' && last t /= '"' = mergeUntilQuote t ts acc
            | otherwise = go False (t : acc) ts
        go True acc ts = reverse acc ++ ts

        mergeUntilQuote :: String -> [String] -> [String] -> [String]
        mergeUntilQuote current [] acc = reverse (current : acc)
        mergeUntilQuote current (t:ts) acc
            | last t == '"' = go False ((current ++ " " ++ t) : acc) ts
            | otherwise     = mergeUntilQuote (current ++ " " ++ t) ts acc

-- | Strip comments from a line.
--------------------------------------
stripComments :: String -> String
stripComments = takeWhile (/= '-') . takeWhile (/= '\n')

--------------------------------------
--          Error Handling          --
--------------------------------------

-- | Handle the result of processing tokens.
handleResult :: (Stack -> IO ()) -> Result -> IO ()

-- Success.
handleResult repl (Success []) = repl []
handleResult repl (Success stack) = do
    putStrLn (prettyPrint stack)
    repl stack

-- Error.
handleResult repl (Error [] "Empty stack") = repl []
handleResult repl (Error stack error') = do 
    putStrLn (prettyPrint stack)
    putStrLn ("Error: " ++ error')
    repl stack

-- Warning.
handleResult repl (Warning stack warning') = do 
    putStrLn (prettyPrint stack)
    putStrLn ("Warning: " ++ warning')
    repl stack

-- Info.
handleResult repl (Info stack info') = do
    putStrLn info'
    repl stack