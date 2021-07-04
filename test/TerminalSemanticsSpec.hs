{-# LANGUAGE NoImplicitPrelude #-}
module TerminalSemanticsSpec (spec) where

import           Import
import           TerminalSemantics
import           TerminalSyntax
import           Test.Hspec
import qualified Text.ParserCombinators.Parsec as P


execCliString :: String -> AppMonad CmdOutput
execCliString str = do
    str' <- evalVars str
    case P.parse cliParser "" str' of
        Left _err -> return $ Failure ""
        Right cli -> execCli cli

execute :: String -> IO CmdOutput
execute cli = newApp >>= flip runApp (execCliString cli)

executeMany :: [String] -> IO [CmdOutput]
executeMany clis = newApp >>= flip runApp (forM clis execCliString)


spec :: Spec
spec = do
    describe "haskell-terminal" $ do
        it "echo passes output to cat" $ execute
            "echo \"abc\" | cat"
            `shouldReturn`
            Success "abc"

        it "variable exporting and interpolation" $ executeMany
            ["export varName=\"aaaa\"", "export otherVar=\"bbb$varName\"", "echo \"$otherVar\""]
            `shouldReturn`
            [Success "", Success "", Success "bbbaaaa"]

        it "checking grep and wc" $ execute
            "grep \"e\" \"test-data/file1.txt\" | wc"
            `shouldReturn`
            Success "5"

        it "grep flags -A and -B work properly" $ execute
            "grep ve \"test-data/file1.txt\" -A 1 -B 1"
            `shouldReturn`
            Success "four\nfive\nsix\n--\nsix\nseven\neight\n"

        it "shell works" $ execute
            "shell ls | grep test-data"
            `shouldReturn`
            Success "test-data\n"

        it "commands can be interpolated" $ executeMany
            ["export echoVar=echo", "$echoVar \"echo this\""]
            `shouldReturn`
            [Success "", Success "echo this"]

        it "parentheses behave as expected" $ execute
            "echo \" | \""
            `shouldReturn`
            Success " | "

        it "supports inline comments" $ execute
            "echo hello \"#world\" # howdy"
            `shouldReturn`
            Success "hello #world"
