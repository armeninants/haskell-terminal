module Main (main) where

import Import
import Run


main :: IO ()
main = flip runApp run =<< newApp
