module ToyPrograms where

import           Options.Applicative.Simple
import           RIO
import           RIO.List
import qualified System.IO                  as IO
import           System.Process             (readProcess)
import           TerminalSyntax
import           Text.Regex.TDFA


handleArgs
    :: String               -- ^ command's name
    -> Parser opts          -- ^ parses arguments into options
    -> InfoMod opts
    -> [String]             -- ^ arguments
    -> (opts -> Program)    -- ^ main program
    -> Program

handleArgs name parser infoMode args prog =
    let
        opts = info (parser <**> helper) infoMode
        resp = execParserPure defaultPrefs opts args
    in case resp of
        Success options -> prog options
        Failure flr ->
            let (flrStr, ec) = renderFailure flr name
            in  const $ (if ec == ExitSuccess then return else pThrowError) flrStr
        _ -> const $ pThrowError "Options are incorrect."


------------------------------------------------------------
-- Echo

echoProgram :: [String] -> Program
echoProgram args inStr =
    if null args
    then return inStr
    else return $ unwords args


------------------------------------------------------------
-- Cat

catProgram :: [String] -> Program
catProgram args inStr =
    if null args
    then return inStr
    else pSafeIO (unlines <$> forM args IO.readFile)


------------------------------------------------------------
-- Export

exportProgram :: [String] -> Program
exportProgram args _inStr = case args of
    var:val:_ -> pSetEnv var val
    _         -> pThrowError "Not enough arguments"


------------------------------------------------------------
-- Grep

data GrepOptions
    = GrepOptions
        { goA      :: Int
        , goB      :: Int
        , goRegExp :: String
        , goFiles  :: [String]
        }
  deriving (Show)


grepOptsParser :: Parser GrepOptions
grepOptsParser = GrepOptions
    <$> option auto
        (   short 'A'
         <> help "After context"
         <> value 0
        )
    <*> option auto
        (   short 'B'
         <> help "Before context"
         <> value 0
        )
    <*> strArgument (metavar "PATTERN")
    <*> many (strArgument (metavar "FILES..."))


slice :: Int -> Int -> [a] -> [a]
slice iFrom iTo xs = take (iTo - iFrom + 1) (drop iFrom xs)


grepProgram :: [String] -> Program
grepProgram args = handleArgs "grep" grepOptsParser infoMod args runGrep
    where
        infoMod
            =  header "Toy implementation of Unix `grep`."
            <> progDesc "echo \"one\\ntwo\\nthree\\nfour\" | grep th"
        runGrep GrepOptions{..} inStr = do
            text <-
                if null goFiles
                then return [inStr]
                else pSafeIO $ forM goFiles IO.readFile
            x <- concat <$> forM text runGrep_
            return $ intercalate sep x where
                sep = if goA + goB > 0 then replicate 2 '-' ++ "\n" else ""
                runGrep_ txt = return withContext where
                    ls = lines txt
                    matched = filter ((=~ goRegExp) . fst) $ zip ls [0..]
                    withContext = map (\x -> unlines $ takeLines goB goA (snd x) ls) matched

        takeLines b a i l = slice (max 0 $ i-b) (min (length l - 1) $ i+a) l


------------------------------------------------------------
-- Shell

shellProgram :: [String] -> Program
shellProgram args _inStr = pSafeIO $ readProcess "sh" ["-c", unwords args] ""


------------------------------------------------------------
-- Wc

wcProgram :: [String] -> Program
wcProgram args inStr =
    if null args
    then return $ countWords inStr
    else pSafeIO $ unlines <$> forM args fileHandler
    where
        countWords =  show . length . words
        fileHandler = (fmap . fmap) countWords IO.readFile
