{-# LANGUAGE OverloadedStrings #-}

module RunProcess (
  runProcess
  , runProcess2
  , runProcess3
  , callCommandWithArgs
) where

import Control.Exception (ioError)
import System.Process hiding (runProcess)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import System.IO
import System.IO.Error (ioError, mkIOError)
import GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )

import System.IO.Temp (withTempFile)
import GHC.IO.Handle (hDuplicate)
import System.Exit (ExitCode(..))
import qualified Control.Exception as C
import System.Directory (removeFile)
import Data.Time.Clock (getCurrentTime,diffUTCTime)

reportHandleStatus label handle = do
  closed <- hIsClosed handle
  putStrLn $ label ++ (if closed then "closed" else "open")

runProcess cmd args input outHandle = do
  ec <- C.bracket
          -- acquire
          ( do (path,handle) <- openTempFile "." "temp-XXXXXXX";
               putStrLn $ "temp file is " ++ path
               return (path,handle) )
          -- clean-up
          (\(path,handle) -> do hClose handle; removeFile path )
          -- body
          (\(path,handle) -> do BS.hPut handle input
                                hSeek handle AbsoluteSeek 0
                                outHandle2 <- hDuplicate outHandle
                                reportHandleStatus "before cp, outHandle2: " outHandle2
                                let cp = (proc cmd args) { std_in = UseHandle handle, std_out = UseHandle outHandle2 }
                                (_,_,_,ph) <- createProcess cp
                                ec <- waitForProcess ph
                                reportHandleStatus "after cp, outHandle2: " outHandle2
                                reportHandleStatus "after cp, handle: " handle
                                return ec
                                )
  putStrLn $ "exit code = " ++ show ec
  return ec

runProcess2 cmd args input = do
  withTempFile "." "temp-XXXXXXXX" $ \outPath outHandle -> do
    ec <- runProcess cmd args input outHandle
    hSeek outHandle AbsoluteSeek 0
    output <- BS.hGetContents outHandle
    return (ec, output)

runProcess3 :: FilePath -> [String] -> ByteString -> IO (ExitCode, Double, ByteString)
runProcess3 cmd args input = do
  withTempFile "." ".tmpout-XXXXXX" $ \outPath outHandle -> do
    withTempFile "." ".tmpin-XXXXXX" $ \inPath inHandle -> do
      BS.hPut inHandle input
      hSeek inHandle AbsoluteSeek 0
      outHandle2 <- hDuplicate outHandle
      let cp = (proc cmd args) { std_in = UseHandle inHandle, std_out = UseHandle outHandle2 }

      start <- getCurrentTime
      (_,_,_,ph) <- createProcess cp
      ec <- waitForProcess ph
      end <- getCurrentTime

      hSeek outHandle AbsoluteSeek 0
      output <- BS.hGet outHandle (1024*1024) -- read at most a megabyte

      let elapsed = (fromRational $ toRational $ diffUTCTime end start) :: Double
      return (ec, elapsed, output)

-- | call a command with args; 
callCommandWithArgs :: FilePath -> [String] -> IO ()
callCommandWithArgs cmd args = do
  ph <- spawnProcess cmd args 
  exitCode <- waitForProcess ph
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure n -> ioError $ mkIOError OtherError "callCommandWithArgs" Nothing (Just cmd)

