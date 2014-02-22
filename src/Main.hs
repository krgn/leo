module Main(main) where

import Web.Leo(query)
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    let s = map ("arg: " ++) args
    putStrLn $ unlines s
