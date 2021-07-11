module ToyPrograms where


import qualified Data.ByteString.Char8      as B
import           Options.Applicative.Simple
import           RIO
import           TerminalSyntax
import           Text.Regex.TDFA


handleArgs
    :: String                   -- ^ command's name
    -> Parser opts              -- ^ parses arguments into options
    -> InfoMod opts
    -> [String]                 -- ^ arguments
    -> (opts -> ProgramM ())    -- ^ main program
    -> ProgramM ()

handleArgs name parser infoMode args prog =
    let
        opts = info (parser <**> helper) infoMode
        resp = execParserPure defaultPrefs opts args
    in case resp of
        Success options -> prog options
        Failure flr ->
            let (flrStr, ec) = renderFailure flr name
            in  (if ec == ExitSuccess then pYield else pThrowStr) $ fromString flrStr
        _ -> pThrowStr "Options are incorrect."


------------------------------------------------------------
-- Echo

echoProgram :: [String] -> ProgramM ()
echoProgram args =
    if null args
    then pAwait >>= pYield . fromMaybe ""
    else pYield . fromString $ unwords args


------------------------------------------------------------
-- Cat

catProgram :: [String] -> ProgramM ()
catProgram args =
    if null args
    then pAwait >>= pYield . fromMaybe ""
    else pSafeIO (B.unlines <$> forM args B.readFile) >>= pYield


------------------------------------------------------------
-- Export

exportProgram :: [String] -> ProgramM ()
exportProgram args = case args of
    var:val:_ -> pSetEnv var val
    _         -> pThrowStr "Not enough arguments"


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


grepProgram :: [String] -> ProgramM ()
grepProgram args = handleArgs "grep" grepOptsParser infoMod args runGrep
    where
        infoMod
            =  header "Toy implementation of Unix `grep`."
            <> progDesc "echo \"one\\ntwo\\nthree\\nfour\" | grep th"
        runGrep GrepOptions{..} = do
            text <-
                if null goFiles
                then pAwait <&> ((:[]) . fromMaybe "")
                else pSafeIO (forM goFiles B.readFile)
            x <- fmap B.concat <$> forM text runGrep_
            pYield $ B.intercalate sep x where
                sep = if goA + goB > 0 then "--\n" else ""
                runGrep_ txt = return withContext where
                    ls = B.lines txt
                    matched = filter ((=~ goRegExp) . fst) $ zip ls [0..]
                    withContext = map (\x -> B.unlines $ takeLines goB goA (snd x) ls) matched

        takeLines b a i l = slice (max 0 $ i-b) (min (length l - 1) $ i+a) l


------------------------------------------------------------
-- Shell

-- shellProgram :: [String] -> ProgramM ()
-- shellProgram args = do
--     pSafeIO (readCreateProcess (shell $ unwords args) "")  >>= pYield


------------------------------------------------------------
-- Wc

wcProgram :: [String] -> ProgramM ()
wcProgram args =
    if null args
    then pAwait >>= pYield . countWords . fromMaybe ""
    else pSafeIO (B.unlines <$> forM args fileHandler) >>= pYield
    where
        countWords =  fromString . show . length . B.words
        fileHandler = (fmap . fmap) countWords B.readFile
