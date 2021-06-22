module Main (main) where

import Import
import Run

main :: IO ()
main = do
    app <- newApp
    runRIO app run
