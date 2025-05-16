
-- File: Runtime.hs
--------------------------------------

--------------------------------------
--             Exports              --
--------------------------------------

module Runtime ( 
    handleResult,
    runFile
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
    processTokens
    )

--------------------------------------
--            Functions             --
--------------------------------------

-- | Runs a file and processes its content.
runFile :: FilePath -> IO ()
runFile path = do
    content <- readFile path
    let tokens = words content
    handleResult (\_ -> return ()) (processTokens tokens [])

-- | Pretty print the stack.
prettyPrint :: [Value] -> String
prettyPrint [v] = show v
prettyPrint vs = show vs

--------------------------------------
--          Error Handling          --
--------------------------------------

-- | Handle the result of processing tokens.
handleResult :: (Stack -> IO ()) -> Result -> IO ()

-- Success.
handleResult repl (Success stack) = do
    putStrLn (prettyPrint stack)
    repl stack

-- Error.
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
    putStrLn (prettyPrint stack)
    putStrLn ("Info: " ++ info')
    repl stack
