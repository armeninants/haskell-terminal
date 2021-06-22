{-# LANGUAGE NoImplicitPrelude #-}
module TerminalSemanticsSpec (spec) where

import           Import
import           TerminalSemantics
import qualified Text.ParserCombinators.Parsec as P
import           TerminalSyntax
import           VirtualCli.Export
import           Test.Hspec


execCliString :: String -> RIO App CmdOutput
execCliString str = do
    str' <- interpolate str
    case P.parse cliParser "" str' of
        Left _err -> return $ Failure ""
        Right cli -> execCli cli

execute :: String -> IO CmdOutput
execute cli = newApp >>= flip runRIO (execCliString cli)

executeMany :: [String] -> IO [CmdOutput]
executeMany clis = newApp >>= flip runRIO (forM clis execCliString)


cmd1 :: String
cmd1 = "echo \"abc\" | cat"

cmd2 :: String
cmd2 = "export varName=\"aaaa\""

cmd3 :: String
cmd3 = "export otherVar=\"bbb$varName\""

cmd4 :: String
cmd4 = "echo \"$otherVar\""

cmd5 :: String
cmd5 = "grep \"e\" \"test-data/file1.txt\" | wc"

cmd6 :: String
cmd6 = "grep ve \"test-data/file1.txt\" -A 1 -B 1"

cmd7 :: String
cmd7 = "shell ls | grep test-data"

spec :: Spec
spec = do
    describe "haskell-terminal" $ do
        it "echo passes output to cat" $ execute cmd1 `shouldReturn` Success "abc"
        it "variable exporting and interpolation" $ executeMany [cmd2,cmd3,cmd4] `shouldReturn` [Success "", Success "", Success "bbbaaaa"]
        it "checking grep and wc" $ execute cmd5 `shouldReturn` Success "5"
        it "grep flags -A and -B work properly" $ execute cmd6 `shouldReturn` Success "four\nfive\nsix\n--\nsix\nseven\neight\n"
        it "shell works" $ execute cmd7 `shouldReturn` Success "test-data\n"


