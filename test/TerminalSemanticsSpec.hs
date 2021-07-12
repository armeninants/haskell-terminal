module TerminalSemanticsSpec (spec) where

import RIO
import TerminalSemantics
import TerminalSyntax
import Test.Hspec


execute :: String -> IO (Either TError TData)
execute = runTerminal . terminalTestApp


executeMany :: [String] -> IO [Either TError TData]
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
            Right "four\nfive\nsix\nsix\nseven\neight\n"

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

        it "shell produces output" $ execute
            "shell echo test"
            `shouldReturn`
            Right "test\n"

        -- it "shell receives input through stdin" $ execute
        --     "echo hello | shell cat"
        --     `shouldReturn`
        --     Right "hello"
