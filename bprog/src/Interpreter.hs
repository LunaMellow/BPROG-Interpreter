
-- File: Interpreter.hs
--------------------------------------

--------------------------------------
--             Exports              --
--------------------------------------

module Interpreter ( 
    repl
) where

--------------------------------------
--             Imports              --
--------------------------------------

-- Local.

import Types (
    Stack
    )

import Runtime (
    handleResult
    )

import Parser (
    processTokens
    )

-- External.

import System.IO (
    hFlush, 
    stdout
    )

--------------------------------------
--            Functions             --
--------------------------------------

-- | Takes a stack and starts a REPL (Read-Eval-Print Loop).
repl :: Stack -> IO ()
repl stack = do

    -- Show interpreter active and flush the output.
    putStr "<bprog> "
    _ <- hFlush stdout

    -- Read tokens from user input.
    line <- getLine
    let tokens = words line

    -- Process the tokens and get the result
    handleResult repl (processTokens tokens stack)
