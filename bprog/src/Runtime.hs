
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
mergeQuotedStrings = reverse . go False [] []
    where
        go _ acc result [] = if null acc then result else unwords (reverse acc) : result
        go False _ result (t:ts)
            | isStartQuote t && not (isEndQuote t) = go True [t] result ts
            | otherwise = go False [] (t : result) ts
        go True acc result (t:ts)
            | isEndQuote t = go False [] (unwords (reverse (t:acc)) : result) ts
            | otherwise    = go True (t:acc) result ts

        isStartQuote s = not (null s) && head s == '"'
        isEndQuote s   = not (null s) && last s == '"'


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