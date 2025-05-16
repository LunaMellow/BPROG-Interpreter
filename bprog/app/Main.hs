
-- File: Main.hs
--------------------------------------

--------------------------------------
--             Exports              --
--------------------------------------

module Main (
    main
) where

--------------------------------------
--             Imports              --
--------------------------------------

-- Local.

import Interpreter (
    repl
    )

import Runtime (
    runFile
    )

-- External.

import System.Environment (
    getArgs
    )

--------------------------------------
--            Functions             --
--------------------------------------

-- | Main function to determine the entry point of the program.
main :: IO ()
main = do

    -- Get command line argumetns.
    args <- getArgs
    case args of
        [] -> repl []
        [path] -> runFile path
        _ -> putStrLn "Usage: bprog [file]"
