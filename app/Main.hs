module Main (main) where

import RIO
import TerminalSemantics
import TerminalSyntax


main :: IO ()
main = runTerminal terminalApp
