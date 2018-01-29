module Test.Hspec.Formatters.Qualified
    (
      codewars
    ) where

import System.IO (Handle)        
import Control.Exception (SomeException)
import Control.Monad (unless, join, forM_)
import Text.Printf (printf)
import Data.List (intercalate)

import Data.List.Split (splitOn)

import Test.Hspec.Core.Spec (Progress)
import Test.Hspec.Runner (Path)
import Test.Hspec.Formatters (Formatter (..), FormatM,
                              FailureRecord (..), FailureReason (..),
                              getRealTime,
                              formatException,
                              write, writeLine)

codewars :: Formatter
codewars = Formatter
    { headerFormatter = headerFormatter'
    , footerFormatter = footerFormatter'
    , exampleGroupStarted = exampleGroupStarted'
    , exampleGroupDone = exampleGroupDone'
    , examplePending = examplePending'
    , exampleSucceeded = exampleSucceeded'
    , exampleFailed = exampleFailed'
    , exampleProgress = exampleProgress'
    , failedFormatter = failedFormatter'
    }

-- https://hackage.haskell.org/package/hspec-core-2.4.4/docs/Test-Hspec-Core-Formatters.html

-- wrap all tests to support reporting total time and nested groups
headerFormatter' :: FormatM ()
headerFormatter' = do
    writeLine "\n<DESCRIBE::>Tests"

footerFormatter' :: FormatM ()
footerFormatter' = do
    time <- getRealTime
    writeLine ""
    writeLine $ printf "<COMPLETEDIN::>%1.4f" (1000 * time)

-- evaluated before each test group
exampleGroupStarted' :: [String] -> String -> FormatM ()
exampleGroupStarted' = \nesting name -> do
    writeLine ""
    writeLine' $ join $ ["<DESCRIBE::>", name]

-- evaluated after each test group
exampleGroupDone' :: FormatM ()
exampleGroupDone' = writeLine "\n<COMPLETEDIN::>"

-- evaluated after each successful example
exampleSucceeded' :: Path -> FormatM ()
exampleSucceeded' = \(_, requirement) -> do
    writeLine ""
    writeLine' $ join ["<IT::>", requirement]
    writeLine "\n<PASSED::>Test Passed"
    writeLine "\n<COMPLETEDIN::>"

-- evaluated after each failed example
exampleFailed' :: Path -> Either SomeException FailureReason -> FormatM ()
exampleFailed' = \(_, requirement) reason -> do
    writeLine ""
    writeLine' $ join ["<IT::>", requirement]
    writeLine ""
    formatFailure reason
    writeLine "\n<COMPLETEDIN::>"

-- evaluated after each pending example
examplePending' :: Path -> Maybe String -> FormatM ()
examplePending' = \_ _ -> return ()

-- Failed test summary
-- evaluated after a test run
failedFormatter' :: FormatM ()
failedFormatter' = return ()

-- used to notify the progress of the currently evaluated example
-- Only called when interactive/color mode.
exampleProgress' :: Handle -> Path -> Progress -> IO ()
exampleProgress' = \_ _ _ -> return ()

writeLine' :: String -> FormatM ()
writeLine' s = writeLine $ intercalate "<:LF:>" $ splitOn "\n" s

formatFailure :: Either SomeException FailureReason -> FormatM ()
formatFailure (Left e) = do
    writeLine ""
    writeLine' $ ((printf "<ERROR::>%s") . formatException) e
formatFailure (Right NoReason) = do
    writeLine ""
    writeLine "<FAILED::>Test Failed"
formatFailure (Right (Reason err)) = do
    writeLine ""
    writeLine' $ (printf "<FAILED::>%s" err)
formatFailure (Right (ExpectedButGot preface expected actual)) = do
    writeLine ""
    mapM_ writeLine preface
    writeLine' $ "<FAILED::>Test Failed\nexpected: " ++ expected ++ "\n but got: " ++ actual
