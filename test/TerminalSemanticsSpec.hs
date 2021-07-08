{-# LANGUAGE NoImplicitPrelude #-}
module TerminalSemanticsSpec (spec) where

import RIO
import TerminalSemantics
import TerminalSyntax
import Test.Hspec


execute :: String -> IO (Either String String)
execute = runTerminal . terminalTestApp


executeMany :: [String] -> IO [Either String String]
executeMany = runTerminal . mapM terminalTestApp


spec :: Spec
spec = do
    describe "haskell-terminal" $ do
        it "echo passes output to cat" $ execute
            "echo \"abc\" | cat"
            `shouldReturn`
            Right "abc"

        it "variable exporting and interpolation" $ executeMany
            ["export varName=\"aaaa\"", "export otherVar=\"bbb$varName\"", "echo \"$otherVar\""]
            `shouldReturn`
            [Right "", Right "", Right "bbbaaaa"]

        it "checking grep and wc" $ execute
            "grep \"e\" \"test-data/file1.txt\" | wc"
            `shouldReturn`
            Right "5"

        it "grep flags -A and -B work properly" $ execute
            "grep ve \"test-data/file1.txt\" -A 1 -B 1"
            `shouldReturn`
            Right "four\nfive\nsix\n--\nsix\nseven\neight\n"

        it "shell works" $ execute
            "shell ls | grep test-data"
            `shouldReturn`
            Right "test-data\n"

        it "commands can be interpolated" $ executeMany
            ["export echoVar=echo", "$echoVar \"echo this\""]
            `shouldReturn`
            [Right "", Right "echo this"]

        it "parentheses behave as expected" $ execute
            "echo \" | \""
            `shouldReturn`
            Right " | "

        it "supports inline comments" $ execute
            "echo hello \"#world\" # howdy"
            `shouldReturn`
            Right "hello #world"
