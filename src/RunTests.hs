{-# LANGUAGE OverloadedStrings #-}

import RunProcess (runProcess3, callCommandWithArgs)

import Control.Monad
import Data.List (foldl', takeWhile, break, unfoldr, intercalate)
import Text.Printf
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified System.IO as IO
import Data.Monoid ((<>), mempty)
import System.Exit (ExitCode(..),exitWith)
import System.FilePath
import Data.Char (isDigit)
import System.Directory (executable,getPermissions,doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError)

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Options.Applicative

-- import Text.PrettyPrint.Boxes

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)

data TableRow3 = TR !ByteString !ByteString !ByteString

colWidths :: TableRow3 -> (Int,Int,Int)
colWidths (TR c1 c2 c3) = (BS.length c1, BS.length c2, BS.length c3)

renderTable :: TableRow3 -> [TableRow3] -> ByteString
renderTable titles rows =
  let (w1,w2,w3)= foldl' (\(a1,a2,a3) tr -> let (b1,b2,b3) = colWidths tr in (max a1 b1, max a2 b2, max a3 b3)) (colWidths titles) rows
      fmt (TR c1 c2 c3) = BS.concat [ ljust w1 c1, " ", ljust w2 c2, " | ",  ljust w3 c3, "\n" ]
      ljust w s = let len = BS.length s in if w > len then BS.append s (BS.replicate (w - len) ' ') else s
      hrule = BS.concat [ BS.replicate w1 '-', "-+-", BS.replicate w2 '-', "-+-", BS.replicate w3 '-', "\n" ]
  in BS.concat $ [fmt titles] ++ map fmt rows

test1 =
  let table = [ TR "this" "is" "a", TR "very good" "test" "of the" ]
  in BS.putStr $ renderTable (TR "Col1" "Column2" "C3") table

compareOutput :: [ByteString] -> [ByteString] -> [(Bool, TableRow3)]
compareOutput [] []           = []
compareOutput xs@[] ys@(y:yt) = (False, TR "~~~" "" y) : compareOutput [] yt
compareOutput xs@(x:xt) []    = (False, TR "~~~" x "") : compareOutput xt []
compareOutput (x:xt) (y:yt)   = (eq, TR col1 x y) : compareOutput xt yt
  where eq = x == y
        col1 = if eq then "" else "!!!"

test2 =
  let got = ["this is", "a test of the", "emergency broadcasting", "system" ]
      exp = ["this is", "a test of", "the emergency broadcasting", "system" ]
      cmp = compareOutput got exp
      rows = map snd cmp
  in BS.putStr $ renderTable (TR "" "Got" "Expected") rows

-- parse a test fle into a list of TestCase structures

data TestCase = TC { tcWhere_ :: Int, tcInput_ :: [ByteString], tcOutput_ :: [ByteString] }
  deriving (Show)

inputLine = BS.isPrefixOf "input:"
outputLine = BS.isPrefixOf "output:"

tcInputBS :: TestCase -> ByteString
tcInputBS testCase = BS.concat $ map (`BS.append` "\r\n") (tcInput_ testCase)

parseTestCase :: [(Int,ByteString)] -> Maybe (TestCase, [(Int,ByteString)])
parseTestCase [] = Nothing
parseTestCase bs =
  -- skip to the next "input:" line
  case dropWhile (not . inputLine . snd) bs of
    []      -> Nothing
    (bs0:bs1) ->
      let (inp, bs2)  = break (outputLine . snd) bs1
          (out, bs3)  = break (inputLine . snd) bs2
          out1 = if null out then [] else tail out
          tc = TC { tcWhere_ = fst bs0, tcInput_ = map snd inp, tcOutput_ = map snd out1 }
      in Just $ (tc, bs3)

parseTestCases :: [(Int,ByteString)] -> [TestCase]
parseTestCases = unfoldr parseTestCase

-- split on \s*\n
trimmedLines :: ByteString -> [ByteString]
trimmedLines ps
  | BS.null ps = []
  | otherwise = let n = fromMaybe (BS.length ps) $ BS.elemIndex '\n' ps
                    go i | i < 0 = i
                         | isSpace (BS.index ps i) = go (i-1)
                         | otherwise = i
                    n' = go (n-1)
                in BS.take (n'+1) ps : trimmedLines (BS.drop (n+1) ps)

readTestFile path = do
  lns <- fmap BS.lines $ BS.readFile path
  let testCases = parseTestCases (zip [1..] lns)
  return testCases

summarizeTestCases :: [TestCase] -> IO ()
summarizeTestCases tcs = do
  putStrLn $ "Number of test cases: " ++ show (length tcs)
  forM_ (zip [(1::Int)..] tcs) $ \(i,tc) -> do
    putStrLn $ printf "#%d: input: %2d  output: %2d" i (length $ tcInput_ tc) (length $ tcOutput_ tc)

runTestCase program testCase = do
  let input = tcInputBS testCase
  (ec, elapsed, output) <- runProcess3 "runHaskell" [program] input
  putStrLn $ "exit status: " ++ show ec ++ ", output length: " ++ show (BS.length output)

test3 = do
  testCases <- readTestFile "./in-1223"
  print $ length testCases
  runTestCase "./1223.hs" (head testCases)

flushStdout = IO.hFlush IO.stdout

safeHead :: b -> (a -> b) -> [a] -> b
safeHead b _ [] = b
safeHead _ f (a:_) = f a

reportTest tc elapsed output = do
  let gotLines = trimmedLines output
      cmps = compareOutput (trimmedLines output) (tcOutput_ tc)
      dots len str = if BS.length str > len-3 then BS.take (len-3) str <> "..." else BS.take len str
      summary = safeHead "(no output)" (dots 30) gotLines
      ok = all fst cmps
      okMessage = if ok then "OK" else "NOT OK" :: String
  putStrLn $ printf "%.3f secs %s - %s" elapsed okMessage (BS.unpack summary)
  when (not ok) $ do
    BS.putStr $ renderTable (TR "" "Got" "Expected") (map snd cmps)
  return ok

data OutputOption = NoOutput | TestOutputOnly | ComparedOutput

type OutputOptions = (OutputOption, OutputOption, OutputOption)

data RunStats = RunStats { runStatsCount_ :: !Int
                           , runStatsPassed_ :: !Int
                           , runStatsFailed_ :: !Int
                           , runStatsError_ :: !Int }
  deriving (Show)

-- | put a @ByteString@ to stdout making sure it terminated with a newline.
putStrWithNL :: ByteString -> IO ()
putStrWithNL bs =
  if "\n" `BS.isSuffixOf` bs then BS.putStr bs else BS.putStrLn bs

runTests :: [(Int,TestCase)] -> FilePath -> [String] -> OutputOptions -> IO RunStats
runTests idCases cmd args (outError, outPassed, outFailed) = do
  let loop stats (i, tc) = do
        putStr $ printf "Test #%d at line %d: " i (tcWhere_ tc)
        flushStdout
        let input = tcInputBS tc
        (ec, elapsed, output) <- runProcess3 cmd args input
        let gotLines = trimmedLines output
            cmps = compareOutput (trimmedLines output) (tcOutput_ tc)
            dots len str = if BS.length str > len-3 then BS.take (len-3) str <> "..." else BS.take len str
            summary = safeHead "(no output)" (dots 30) gotLines
        let (err, ok, okMessage) =
              case ec of
                ExitFailure n -> (True, False, "EXIT " ++ show n)
                ExitSuccess   -> let ok = all fst cmps
                                     str = if ok then "OK" else "NOT OK"
                                 in (False, ok, str)
            whichOut = case (err, ok) of
                         (True, _)      -> outError
                         (False, True)  -> outPassed
                         (False, False) -> outFailed
        putStrLn $ printf "%.3f secs %s - %s" elapsed okMessage (BS.unpack summary)
        case whichOut of
          NoOutput       -> return ()
          TestOutputOnly -> putStrWithNL output
          ComparedOutput -> BS.putStr $ renderTable (TR "" "Got" "Expected") (map snd cmps)
        let stats' = case (err, ok) of
                       (True, _)     -> stats { runStatsError_ = (runStatsError_ stats)+1
                                                , runStatsCount_ = (runStatsCount_ stats)+1 }
                       (False,True)  -> stats { runStatsPassed_ = (runStatsPassed_ stats)+1
                                                , runStatsCount_ = (runStatsCount_ stats)+1 }
                       (False,False) -> stats { runStatsFailed_ = (runStatsFailed_ stats)+1
                                                , runStatsCount_ = (runStatsCount_ stats)+1 }
        return stats'
  stats <- foldM loop (RunStats 0 0 0 0) idCases
  return stats

system cmd args = do
  putStrLn $ intercalate " " $ ["+", cmd] ++ args
  callCommandWithArgs cmd args

reportStats :: RunStats -> String
reportStats stats
  | errors > 0 = printf "Passed: %d / %d, Errors: %d" passed count errors
  | otherwise  = printf "Passed: %d / %d" passed count
  where passed = runStatsPassed_ stats
        count = runStatsCount_ stats
        errors = runStatsError_ stats

data ProgramType = Executable | HaskellSource | CSource | CPPSource
  deriving (Show)

endsInNumber str =
  let (ds,rest) = span isDigit (reverse str)
  in if length ds == 4
       then Just (reverse rest, reverse ds)
       else Nothing

classifyProgram :: String -> Maybe (Int, ProgramType, String)
classifyProgram path = do
  let ext = takeExtension path
      base = takeBaseName path
  -- base must end in a 4-digit number
  (prefix, problemId) <- endsInNumber base
  typ <- case ext of
           ""     -> Just Executable
           ".exe" -> Just Executable
           ".hs"  -> Just HaskellSource
           ".c"   -> Just CSource
           ".cpp" -> Just CPPSource
           _      -> Nothing
  let pre = takeDirectory path </> prefix
  return (read problemId, typ, pre)

warn str = hPutStrLn stderr str

isExecutable :: FilePath -> IO Bool
isExecutable path =
  catchIOError
    (getPermissions path >>= (return . executable))
    handler
  where handler e = do warn $ path <> ": " <> show e; return False

prepareExecutable :: ProgramType -> FilePath -> IO FilePath
prepareExecutable ptype src = do
  let objPath = replaceExtension src ""
  case ptype of
    Executable    -> return src -- XXX make sure file is executable
    HaskellSource -> do system "ghc" ["-O2", "--make", src]
                        return objPath
    CSource       -> do system "cc"  [src, "-o", replaceExtension src ""]
                        return objPath
    CPPSource     -> do system "g++" [src, "-o", replaceExtension src ""]
                        return objPath

data TestRange = TRSingle Int | TRRange Int Int | TROpen Int
  deriving (Show)

showTestRange (TRSingle x)  = show x
showTestRange (TRRange x y) = show x ++ "-" ++ show y
showTestRange (TROpen x)    = show x ++ "-"

data Options = Options
  { 
    srcPath        :: FilePath
  , testSelections :: [TestRange]
  , testPath       :: FilePath
  , emitFirstTest  :: Bool
  , emitRawOutput  :: Bool
  }
  deriving (Show)

parseTestSelection str
  | null ds1  = Nothing
  | null str2 = Just $ TRSingle n1
  | null str3 = Just $ TROpen n1
  | null ds2  = Nothing
  | null str4 = Just $ TRRange n1 n2
  | otherwise = Nothing
  where
    (ds1,str2)    = span isDigit str
    (dashes,str3) = span (=='-') str2
    (ds2,str4)    = span isDigit str3
    n1 = read ds1
    n2 = read ds2

notTestSelection str =
  case parseTestSelection str of
    Nothing -> Just str
    Just _  -> Nothing

optionsParser :: Parser Options
optionsParser
   = Options <$> argument notTestSelection (metavar "PROGRAM")
             <*> many (argument parseTestSelection (metavar "TEST-SELECTIONS"))
             <*> strOption (short 't' <> metavar "TESTFILE" <> value "")
             <*> switch (short 'x')
             <*> switch (short 'v')

isInRange :: Int -> TestRange -> Bool
isInRange x (TRSingle a)   = a == x
isInRange x (TRRange a b)  = a <= x && x <= b
isInRange x (TROpen a)     = a <= x

isContainedIn :: Int -> Int -> TestRange -> Bool
isContainedIn lo hi tr =
  case tr of
    TRSingle x  -> rng x
    TROpen x    -> rng x
    TRRange x y ->  rng x && rng y
  where rng x = lo <= x && x <= hi

selectTests :: [a] -> [TestRange] -> [(Int,a)]
selectTests as ranges = filter (\(i,a) -> inrange i) $ zip [1..] as
  where inrange x = any (isInRange x) ranges

plural sing plur n = if n == 1 then sing else plur

reportBadRanges :: FilePath -> Int -> [TestRange] -> IO Bool
reportBadRanges testPath n ranges = do
  let bad = filter (not . isContainedIn 1 n) ranges
      hasBad = not $ null bad
  when hasBad $ do
    warn $ "The file \"" <> testPath <> "\" has " <> (show n) <> " " <> plural "test" "tests" n <> "."
    warn $ "The following ranges select non-existent tests: " <> (intercalate ", " $ map showTestRange bad)
  return hasBad

mainWithOpts opts = do
  let path     = srcPath opts
      testpath = testPath opts
      ranges   = testSelections opts
  b <- lift $ doesFileExist path
  unless b $ left $ "File does not exist: " <> path
  (pid, ptype, _) <- case classifyProgram path of
                       Nothing -> left $ "unable to identify file: " <> path
                       Just x  -> right x
  let testPath' = if null testpath
                     then takeDirectory path </> "in-" <> (show pid)
                     else testpath
  b <- lift $ doesFileExist testPath'
  unless b $ left $ "Test file \"" <> testPath' <> "\" does not exist."
  exePath <- lift $ prepareExecutable ptype path
  b <- lift $ isExecutable exePath
  unless b $ left $ "File is not executable: " <> path
  lift $ do
    allTests <- readTestFile testPath'
    let testCount = length allTests
        outOptions = if emitRawOutput opts
                       then (TestOutputOnly, TestOutputOnly, TestOutputOnly)
                       else (NoOutput, NoOutput, ComparedOutput)
        testsToRun = selectTests allTests $ if null ranges then [TROpen 1] else ranges
    reportBadRanges testPath' testCount ranges
    if null testsToRun
      then do putStrLn "No tests selected."
              return ()
      else do if emitFirstTest opts
                then do -- emit the text of the first selected test case
                        let (_,t) = head testsToRun
                        putStrWithNL $ tcInputBS t
                else do stats <- runTests testsToRun exePath [] outOptions 
                        putStrLn $ reportStats stats

main :: IO ()
main = do
  opts <- execParser (info optionsParser mempty)
  err <- runEitherT $ mainWithOpts opts
  case err of
    Left err -> do warn err; exitWith (ExitFailure 1)
    Right _  -> return ()

